package org.apache.commons.net.ftp.ssl;

import java.io.IOException;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.net.ftp.ssl.channel.SecureChannelManager;

public class ChannelSelectorDispatcher implements Runnable {
    private final static Logger mLogger = Logger.getLogger(ChannelSelectorDispatcher.class.getName());
    private Selector selector;
    private final Thread selectorThread;
    private AtomicBoolean closeRequested;
    private final List registerChannelRequests = new ArrayList();
    private final SecureChannelManager sscManager = new SecureChannelManager();
    private final Map<SelectableChannel, SelectionKey> mChannel2SelectionKey = new HashMap<SelectableChannel, SelectionKey>();
    private final Map<SelectableChannel, Semaphore> mChannel2Sema = new HashMap<SelectableChannel, Semaphore>();
    private ReentrantReadWriteLock mRWL;
    private java.util.concurrent.locks.Lock mRL;
    private java.util.concurrent.locks.Lock mWL;
    
    public ChannelSelectorDispatcher() throws IOException {
        closeRequested = new AtomicBoolean(false);
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
        selector = Selector.open();
        selectorThread = new Thread(this);
        selectorThread.setName("FTP-Channel-Selector-Dispatcher-" + selectorThread.getName());
        selectorThread.start();
    }

    public void addChannelInterest(SelectableChannel channel, int interest)
            throws IOException {
        //SelectionKey sk = channel.keyFor(selector);
        SelectionKey sk = getSelectionKeyForChannel(channel);
        changeKeyInterest(sk, sk.interestOps() | interest);
    }

    public void removeChannelInterest(SelectableChannel channel, int interest)
            throws IOException {
        if (Thread.currentThread() != selectorThread) {
            throw new IOException(
                    "Method can only be called from selector thread");
        }
        //SelectionKey sk = channel.keyFor(selector);
        SelectionKey sk = getSelectionKeyForChannel(channel);
        changeKeyInterest(sk, sk.interestOps() & ~interest);
    }

    private void changeKeyInterest(SelectionKey sk, int newInterest)
            throws IOException {
        try {
            sk.interestOps(newInterest);
        } catch (CancelledKeyException cke) {
            IOException ioe = new IOException(
                    "Failed to change channel interest.");
            ioe.initCause(cke);
            throw ioe;
        }
    }

    public void registerChannel(SelectableChannel channel, int selectionKeys,
            ReadWriteHandler handlerInfo) throws IOException {
        if (Thread.currentThread() != selectorThread) {
            throw new IOException(
                    "Method can only be called from selector thread");
        }

        if (!channel.isOpen()) {
            throw new IOException("Channel is not open.");
        }

        try {
            if (channel.isRegistered()) {
                //SelectionKey sk = channel.keyFor(selector);
                SelectionKey sk = getSelectionKeyForChannel(channel);
                assert sk != null : "Channel is already registered with other selector";
                sk.interestOps(selectionKeys);
                Object previousAttach = sk.attach(handlerInfo);
                assert previousAttach != null;
            } else {
                channel.configureBlocking(false);
                //SelectionKey sk = channel.register(selector, selectionKeys, handlerInfo);
                // to work around a GlassFish ASSelector issue (channel.keyFor(selector) does not return the key)
                // we have to remember the registered channel + key, so that simulate the
                // keyFor() using register()
                SelectionKey sk = registerChannelWithSelector(channel, selector, selectionKeys, handlerInfo);
            }
        } catch (Exception e) {
            IOException ioe = new IOException("Error registering channel.");
            ioe.initCause(e);
            throw ioe;
        }
    }

    public void deregisterChannel(final SelectableChannel channel) throws IOException {
        if (!channel.isOpen()) {
            throw new IOException("Channel is not open.");
        }

        try {
            if (channel.isRegistered()) {
                SelectionKey sk = getSelectionKeyForChannel(channel);
                //SelectionKey sk = channel.keyFor(selector);
                sk.cancel();
                removeSelectionKeyForChannel(channel);
            }
        } catch (Exception e) {
            IOException ioe = new IOException("Error deregistering channel.");
            ioe.initCause(e);
            throw ioe;
        }
    }

    public void registerChannelLater(final SelectableChannel channel,
            final int selectionKeys, final ReadWriteHandler handlerInfo,
            final ErrorHandler errorHandler) {
        invokeLater(new Runnable() {

            public void run() {
                try {
                    registerChannel(channel, selectionKeys, handlerInfo);
                } catch (IOException e) {
                    errorHandler.handleError(e);
                }
            }
        });
    }

    public void invokeLater(Runnable run) {
        synchronized (registerChannelRequests) {
            registerChannelRequests.add(run);
        }
        selector.wakeup();
    }

    private void processChannelRegisterRequests() {
        synchronized (registerChannelRequests) {
            for (int i = 0; i < registerChannelRequests.size(); i++) {
                Runnable task = (Runnable) registerChannelRequests.get(i);
                task.run();
            }
            registerChannelRequests.clear();
        }
    }

    private SelectionKey getSelectionKeyForChannel(SelectableChannel channel) {
        SelectionKey sk = null;
        mRL.lock();
        try {
            sk = mChannel2SelectionKey.get(channel);
        }
        finally {
            mRL.unlock();
        }
        return sk;
    }
    
    private SelectionKey removeSelectionKeyForChannel(SelectableChannel channel) {
        SelectionKey sk = null;
        mWL.lock();
        try {
            sk = mChannel2SelectionKey.remove(channel);
            Semaphore sema = mChannel2Sema.remove(channel);
            if ( sema != null )
                sema.release(); // in case some one is blocked - free it
        }
        finally {
            mWL.unlock();
        }
        return sk;
    }

    private SelectionKey registerChannelWithSelector(
            SelectableChannel channel, 
            Selector selector, 
            int selectionKeys,
            ReadWriteHandler handlerInfo) throws ClosedChannelException {
        SelectionKey sk = null;
        mWL.lock();
        try {
            sk = channel.register(selector, selectionKeys, handlerInfo);
            mChannel2SelectionKey.put(channel, sk);
            Semaphore sema = mChannel2Sema.get(channel);
            if ( sema == null ) {
                if ( mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "sema not found, registerChannelWithSelector( channel=" + channel + ", selector=" + selector + " selection keys =" + selectionKeys + " handler=" + handlerInfo + ")");
                }
                // no one is waiting yet
                mChannel2Sema.put(channel, new Semaphore(1)); 
                // do not block next thread that needs the channel registered
                // e.g. to do IO on the channel
            }
            else {
                if ( mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "sema = " + sema + " registerChannelWithSelector( channel=" + channel + ", selector=" + selector + " selection keys =" + selectionKeys + " handler=" + handlerInfo + ")");
                }
                sema.release();
                if ( mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "released sema = " + sema + " registerChannelWithSelector( channel=" + channel + ", selector=" + selector + " selection keys =" + selectionKeys + " handler=" + handlerInfo + ")");
                }
            }
        }
        finally {
            mWL.unlock();
        }
        if ( mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "selection key returned sk = " + sk + " registerChannelWithSelector( channel=" + channel + ", selector=" + selector + " selection keys =" + selectionKeys + " handler=" + handlerInfo + ")");
        }
        return sk;
    }

    public void waitForRegisterDone(SelectableChannel channel) {
        Semaphore sema = null;
        mWL.lock();
        try {
            sema = mChannel2Sema.get(channel);
            if ( sema == null ) {
                if ( mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "sema not found waitForRegisterDone( channel=" + channel + ")");
                }

                if ( mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "sema not found waitForRegisterDone( channel=" + channel + ")");
                }
                // not registered yet
                mChannel2Sema.put(channel, (sema = new Semaphore(0))); 
                // block invoker until registered
            }
        }
        finally {
            mWL.unlock();
        }

        try {
            if ( mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "sema.acquire() in waitForRegisterDone( channel=" + channel + ")");
            }
            sema.acquire();
            if ( mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "sema.acquire() successed in waitForRegisterDone( channel=" + channel + ")");
            }
        }
        catch (Exception e) {
            // ignore
            e.printStackTrace();
            if ( mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "waitForRegisterDone( channel=" + channel + "), exception while sema.acquire()..., e=" + e.getMessage());
            }
        }
    }

    /**
     * shutdown the dispather requested
     */
    public void requestClose() {
        closeRequested.set(true);
        selector.wakeup();
    }

    /**
     * Main cycle. This is where event processing and dispatching happens.
     */
    public void run() {
        while (true) {
            processChannelRegisterRequests();

            if (closeRequested.get()) {
                cleanupRegistries();
                return;
            }

            sscManager.fireEvents();

            int selectedKeys = 0;
            try {
                selectedKeys = selector.select(1000);
            } catch (IOException ioe) {
                ioe.printStackTrace();
                continue;
            }

            if (selectedKeys == 0) {
                continue;
            }

            Iterator it = selector.selectedKeys().iterator();
            while (it.hasNext()) {
                SelectionKey sk = (SelectionKey) it.next();
                it.remove();
                try {
                    int readyOps = sk.readyOps();
                    sk.interestOps(sk.interestOps() & ~readyOps);
                    ReadWriteHandler handler = (ReadWriteHandler) sk.attachment();

                    ReadWriteHandler rwHandler = (ReadWriteHandler) handler;

                    if (sk.isReadable()) {
                        rwHandler.handleRead();
                    }

                    if (sk.isValid() && sk.isWritable()) {
                        rwHandler.handleWrite();
                    }
                } catch (Throwable t) {
                    closeSelectorAndChannels();
                    t.printStackTrace();
                    return;
                }
            }
        }
    }

    private void closeSelectorAndChannels() {
        Set keys = selector.keys();
        for (Iterator iter = keys.iterator(); iter.hasNext();) {
            SelectionKey key = (SelectionKey) iter.next();
            try {
                key.channel().close();
            } catch (IOException e) {
                // Ignore
            }
        }
        try {
            selector.close();
        } catch (IOException e) {
            // Ignore
        }
    }

    public SecureChannelManager getSscManager() {
        return sscManager;
    }

    public Selector getSelector() {
        return selector;
    }
    
    private void cleanupRegistries() {
        mWL.lock();
        try {
            Set chSet = mChannel2SelectionKey.keySet();
            Iterator it = chSet.iterator();
            while ( it.hasNext() ) {
                Object key = it.next();
                SelectionKey sk = mChannel2SelectionKey.get(key);
                try {
                    sk.cancel();
                }
                catch (Exception e) {
                    // ignore
                }
            }
            
            mChannel2SelectionKey.clear();

            chSet = mChannel2Sema.keySet();
            it = chSet.iterator();
            while ( it.hasNext() ) {
                Object key = it.next();
                Semaphore sema = mChannel2Sema.get(key);
                try {
                    sema.release();
                }
                catch (Exception e) {
                    // ignore
                }
            }
            
            mChannel2Sema.clear();
        }
        finally {
            mWL.unlock();
        }
    }
}