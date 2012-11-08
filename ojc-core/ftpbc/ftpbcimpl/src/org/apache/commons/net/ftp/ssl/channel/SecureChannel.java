package org.apache.commons.net.ftp.ssl.channel;

import org.apache.commons.net.ftp.ssl.ErrorHandler;
import org.apache.commons.net.ftp.ssl.ChannelSelectorDispatcher;
import org.apache.commons.net.ftp.ssl.SelectableAliasX509ExtendedKeyManager;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.security.KeyStore;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509KeyManager;

/**
 * this class and some others are adapted from sample code
 * from Nuno Santos
 * 
 * @author jfu
 *
 */
public class SecureChannel implements Channel {

    private final static Logger mLogger = Logger.getLogger(SecureChannel.class.getName());
    protected final ChannelSelectorDispatcher mDispatcher;
    protected final SocketChannel mSocketChannel;
    protected final ChannelListener mChannelListener;
    private final SSLSession mSession;
    private final SSLEngine mEngine;
    private final ByteBuffer mAppBufferClear;
    private final ByteBuffer mAppBufferEncrypt;
    private final ByteBuffer mNetBuffer;
    private boolean bReadInterestFromApp = false;
    private boolean bHasWriteInterestFromApp = false;
    private boolean bHasReadInterestFromChannel = false;
    private boolean bHasWriteInterestFromChannel = false;
    /**
     * flag indicates the duration of a init handshake
     */
    private boolean initialHandshake = false;
    /**
     * SSLEngine status
     */
    private SSLEngineResult.HandshakeStatus mStatus;
    private ByteBuffer mNullBuffer;
    private boolean shutdown = false;
    private boolean closed = false;
    private SSLEngineResult.Status mEngineStatus = null;
    private IOException mLastException = null;

    /**
     * @param dispatcher -
     *            the thread polling on a selector for Read/Write on the
     *            SocketChannels
     * @param channel -
     *            the socket channel underlying
     * @param mChannelListener -
     *            the application communicating with peer on this channel
     * @throws Exception
     */
    private SecureChannel(ChannelSelectorDispatcher dispatcher,
            SocketChannel channel, ChannelListener listener, SSLEngine engine,
            ErrorHandler errorHandler) throws Exception {

        mDispatcher = dispatcher;
        mChannelListener = listener;
        mSocketChannel = channel;
        mEngine = engine;
        mSession = engine.getSession();
        mAppBufferEncrypt = ByteBuffer.allocate(mSession.getPacketBufferSize());
        mAppBufferClear = ByteBuffer.allocate(mSession.getApplicationBufferSize());
        mNetBuffer = ByteBuffer.allocate(mSession.getPacketBufferSize());

        mAppBufferClear.position(mAppBufferClear.limit());
        mNetBuffer.position(mNetBuffer.limit());

        dispatcher.registerChannelLater(channel, 0, this, errorHandler);

        engine.beginHandshake();
        mStatus = engine.getHandshakeStatus();
        initialHandshake = true;
        mNullBuffer = ByteBuffer.allocate(0);
        listener.setSecureChannel(this);
    // this can be racing condition
    // handshake() has to wait until register done;
//		handshake();
    }

    private void validateChannel() throws IOException {
        if (closed) {
            throw new ClosedChannelException();
        }
        if (mLastException != null) {
            IOException ioe = new IOException("Asynchronous failure: " + mLastException.getMessage());
            ioe.initCause(mLastException);
            throw ioe;
        }
    }

    public int read(ByteBuffer dst) throws IOException {

        validateChannel();

        if (initialHandshake) {
            return 0;
        }
        if (mEngine.isInboundDone()) {
            return -1;
        }
        if (!mAppBufferClear.hasRemaining()) {
            int appBytesProduced = decrypt();
            if (appBytesProduced == -1 || appBytesProduced == 0) {
                return appBytesProduced;
            }
        }

        int limit = Math.min(mAppBufferClear.remaining(), dst.remaining());
        for (int i = 0; i < limit; i++) {
            dst.put(mAppBufferClear.get());
        }
        return limit;
    }

    public int getPacketBufSz() {
        return mSession.getPacketBufferSize();
    }

    public int getAppBufSz() {
        return mSession.getApplicationBufferSize();
    }

    private int decrypt() throws IOException {
        int bytesRead = mSocketChannel.read(mAppBufferEncrypt);
        if (bytesRead == -1) {
            if (mAppBufferEncrypt.position() == 0 || mEngineStatus == SSLEngineResult.Status.BUFFER_UNDERFLOW) {
                return -1;
            }
        }

        mAppBufferClear.clear();
        mAppBufferEncrypt.flip();
        SSLEngineResult res;

        do {
            res = mEngine.unwrap(mAppBufferEncrypt, mAppBufferClear);
        } while (res.getStatus() == SSLEngineResult.Status.OK && res.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.NEED_UNWRAP && res.bytesProduced() == 0);

        if (res.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.FINISHED) {
            finishInitialHandshake();
        }

        if (mAppBufferClear.position() == 0 && res.getStatus() == SSLEngineResult.Status.OK && mAppBufferEncrypt.hasRemaining()) {
            res = mEngine.unwrap(mAppBufferEncrypt, mAppBufferClear);
        }

        mEngineStatus = res.getStatus();
        mStatus = res.getHandshakeStatus();

        if (mEngineStatus == SSLEngineResult.Status.CLOSED) {
            shutdown = true;
            shutdown();
            return -1;
        }

        // Prepare the buffer to be written again.
        mAppBufferEncrypt.compact();
        // And the app buffer to be read.
        mAppBufferClear.flip();

        if (mStatus == SSLEngineResult.HandshakeStatus.NEED_TASK || mStatus == SSLEngineResult.HandshakeStatus.NEED_WRAP || mStatus == SSLEngineResult.HandshakeStatus.FINISHED) {
            mLogger.fine("Rehandshaking...");
            handshake();
        }

        return mAppBufferClear.remaining();
    }

    /**
     * 
     */
    public int write(ByteBuffer src) throws IOException {
        validateChannel();
        if (initialHandshake) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine("Writing not possible during handshake");
            // Not ready to write
            }
            return 0;
        }

        // First, check if we still have some data waiting to be sent.
        if (mNetBuffer.hasRemaining()) {
            // There is. Don't try to send it. We should be registered
            // waiting for a write event from the selector thread
            assert bHasWriteInterestFromChannel : "Write interest should be active" + mNetBuffer;
            return 0;
        }
        assert !bHasWriteInterestFromChannel : "Write interest should not be active";

        // There is no data left to be sent. Clear the buffer and get
        // ready to encrypt more data.
        mNetBuffer.clear();
        SSLEngineResult res = mEngine.wrap(src, mNetBuffer);
        mNetBuffer.flip();
        flushData();

        return res.bytesConsumed();
    }

    /**
     * This method may result in a read attempt from the socket.
     */
    public void registerForRead() throws IOException {
        validateChannel();
        if (!bReadInterestFromApp) {
            bReadInterestFromApp = true;
            if (initialHandshake) {
                // Wait for handshake to finish
                return;

            } else {
                if (mAppBufferClear.hasRemaining()) {
                    // There is decrypted data available, so prepare
                    // to fire the read event to the application
                    mDispatcher.getSscManager().registerForRead(this);

                } else {
                    // There is no decrypted data. But there may be some
                    // encrypted data.
                    if (mAppBufferEncrypt.position() == 0 || mEngineStatus == SSLEngineResult.Status.BUFFER_UNDERFLOW) {
                        // Must read more data since either there is no
                        // encrypted
                        // data available or there is data available but not
                        // enough to reassemble a packet.
                        selectorRegisterForRead();
                    } else {
                        // There is encrypted data available. It may or may not
                        // be enough to reassemble a full packet. We have to
                        // check it.
                        if (decrypt() == 0) {
                            // Not possible to reassemble a full packet.
                            selectorRegisterForRead();
                        } else {
                            // either EOF or there is application data ready. In
                            // both
                            // cases we must inform the application
                            mDispatcher.getSscManager().registerForRead(this);
                        }
                    }
                }
            }
        }
    }

    public void unregisterForRead() throws IOException {
        validateChannel();
        bReadInterestFromApp = false;
        mDispatcher.getSscManager().unregisterForRead(this);
    }

    /**
     * Raises an event when there is space available for writing more data to
     * this socket.
     */
    public void registerForWrite() throws IOException {
        validateChannel();
        if (!bHasWriteInterestFromApp) {
            bHasWriteInterestFromApp = true;
            if (initialHandshake) {
                return;
            } else {
                // Check if we can write now
                if (mNetBuffer.hasRemaining()) {
                    assert bHasWriteInterestFromChannel : "Write interest should be active";
                // The buffer is full, the application can't write anymore.
                // The write interest must be set...
                } else {
                    assert !bHasWriteInterestFromChannel : "Write interest should not be active";
                    // netData is empty. But don't fire the write event right
                    // now. Instead, register with the SecureChannelManager and
                    // wait for the SelectorThread to call for these events.
                    mDispatcher.getSscManager().registerForWrite(this);
                }
            }
        }
    }

    /**
     * Cancel the write interest.
     */
    public void unregisterForWrite() throws IOException {
        validateChannel();
        bHasWriteInterestFromApp = false;
        mDispatcher.getSscManager().unregisterForWrite(this);
    }

    /**
     * Called from the SSLChannelManager when it's time for launching the
     * application events
     */
    void fireReadEvent() {
        bReadInterestFromApp = false;
        this.mChannelListener.processInbound();
    }

    /**
     * Called from the SSLChannelManager when it's time for launching the
     * application events
     */
    void fireWriteEvent() {
        bHasWriteInterestFromApp = false;
        this.mChannelListener.processOutbound();
    }

    private void shutdown() throws IOException {
        if (mLastException != null || mEngine.isOutboundDone()) {
            try {
                mSocketChannel.close();
            } catch (IOException e) {
                /* Ignore. */
            }
            return;
        }

        mNetBuffer.clear();

        try {
            SSLEngineResult res = mEngine.wrap(mNullBuffer, mNetBuffer);
        } catch (SSLException e1) {
            try {
                mSocketChannel.close();
            } catch (IOException e) { /* Ignore. */

            }
            return;
        }
        mNetBuffer.flip();
        flushData();
    }

    public void close() throws IOException {
        if (shutdown) {
            return;
        }
        shutdown = true;
        closed = true;
        mLastException = null;
        mEngine.closeOutbound();
        if (mNetBuffer.hasRemaining()) {
            return;
        } else {
            shutdown();
        }
        mChannelListener.notifySSLChannelClosure();
    }

    private void finishInitialHandshake() throws IOException {
        initialHandshake = false;
        if (bReadInterestFromApp) {
            selectorRegisterForRead();
        }
        if (bHasWriteInterestFromApp) {
            mDispatcher.getSscManager().registerForWrite(this);
        }
    }

    private void handshake() throws IOException {
        while (true) {
            SSLEngineResult res;
            switch (mStatus) {
                case FINISHED:
                    if (initialHandshake) {
                        finishInitialHandshake();
                    }
                    return;

                case NEED_TASK:
                    doTasks();
                    break;

                case NEED_UNWRAP:
                    decrypt();
                    if (initialHandshake && mEngineStatus == SSLEngineResult.Status.BUFFER_UNDERFLOW) {
                        selectorRegisterForRead();
                    }
                    return;

                case NEED_WRAP:
                    if (mNetBuffer.hasRemaining()) {
                        return;
                    }

                    mNetBuffer.clear();
                    res = mEngine.wrap(mNullBuffer, mNetBuffer);
                    mStatus = res.getHandshakeStatus();
                    mNetBuffer.flip();

                    if (!flushData()) {
                        return;
                    }
                    break;

                case NOT_HANDSHAKING:
                    System.err.println("doHandshake() should never reach the NOT_HANDSHAKING state");
                    return;
            }
        }
    }

    /**
     * Called from the selector thread.
     */
    public void handleRead() {
        bHasReadInterestFromChannel = false;
        try {
            if (initialHandshake) {
                handshake();

            } else if (shutdown) {
                shutdown();

            } else {
                int bytesUnwrapped = decrypt();
                if (bytesUnwrapped == -1) {
                    mDispatcher.getSscManager().registerForRead(this);

                } else if (bytesUnwrapped == 0) {
                    selectorRegisterForRead();
                } else {
                    mDispatcher.getSscManager().registerForRead(this);
                }
            }
        } catch (IOException e) {
            handleAsynchException(e);
        }
    }

    private boolean flushData() throws IOException {
        int written;
        try {
            written = mSocketChannel.write(mNetBuffer);
        } catch (IOException ioe) {
            mNetBuffer.position(mNetBuffer.limit());
            throw ioe;
        }
        if (mNetBuffer.hasRemaining()) {
            selectorRegisterForWrite();
            return false;
        } else {
            return true;
        }
    }

    public void handleWrite() {
        bHasWriteInterestFromChannel = false;
        try {
            if (flushData()) {
                if (initialHandshake) {
                    handshake();

                } else if (shutdown) {
                    shutdown();

                } else {
                    if (bHasWriteInterestFromApp) {
                        mDispatcher.getSscManager().registerForWrite(this);
                    }
                }
            } else {
            }
        } catch (IOException e) {
            handleAsynchException(e);
        }
    }

    private void handleAsynchException(IOException e) {
        mLastException = e;
        if (bHasWriteInterestFromApp) {
            mDispatcher.getSscManager().registerForWrite(this);
        }
        if (bReadInterestFromApp) {
            mDispatcher.getSscManager().registerForRead(this);
        }
        mEngine.closeOutbound();
    }

    private void selectorRegisterForRead() throws IOException {
        if (bHasReadInterestFromChannel) {
            return;
        }
        bHasReadInterestFromChannel = true;
        mDispatcher.addChannelInterest(mSocketChannel, SelectionKey.OP_READ);
    }

    private void selectorRegisterForWrite() throws IOException {
        if (bHasWriteInterestFromChannel) {
            return;
        }
        bHasWriteInterestFromChannel = true;
        mDispatcher.addChannelInterest(mSocketChannel, SelectionKey.OP_WRITE);

    }

    private void doTasks() {
        Runnable task;
        while ((task = mEngine.getDelegatedTask()) != null) {
            task.run();
        }
        mStatus = mEngine.getHandshakeStatus();
    }

    public SocketChannel getSocketChannel() {
        return mSocketChannel;
    }

    public static SSLContext createSSLContext(String keystore,
            String kpassword, String truststore, String tpassword, String clientAlias, String aliasPass)
            throws Exception {
        KeyStore kstore = null;
        KeyStore tstore = null;

        if (keystore != null && keystore.trim().length() > 0) {
            kstore = KeyStore.getInstance("JKS");
            kstore.load(new FileInputStream(keystore),
                    kpassword != null ? kpassword.toCharArray() : "".toCharArray());
        }

        if (truststore != null && truststore.trim().length() > 0) {
            tstore = KeyStore.getInstance("JKS");
            tstore.load(new FileInputStream(truststore),
                    tpassword != null ? tpassword.toCharArray() : "".toCharArray());
        }

        if (tstore == null) {
            throw new IllegalArgumentException(
                    "trust store information not available.");
        }
        SSLContext sslContext = SSLContext.getInstance("TLS");

        TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
        tmf.init(tstore);

        KeyManagerFactory kmf = null;

        if (kstore != null) {
            kmf = KeyManagerFactory.getInstance("SunX509");
            if (aliasPass != null && aliasPass.trim().length() > 0 ) {
                kmf.init(kstore, aliasPass.toCharArray());
            } else {
                kmf.init(kstore, kpassword != null ? kpassword.toCharArray() : "".toCharArray());
            }
        }

        // prepare key managers
        KeyManager[] kms = kmf != null ? kmf.getKeyManagers() : null;

        if (clientAlias != null && clientAlias.trim().length() > 0) {
            if (kms.length > 0 && kms[0] != null) {
                // if key pass is null or blank - then use the keystore password
                String keyPass = aliasPass != null && aliasPass.trim().length() > 0 ? aliasPass : kpassword;
                SelectableAliasX509ExtendedKeyManager smanager = SelectableAliasX509ExtendedKeyManager.createKeyManager((X509KeyManager) kms[0], kstore, clientAlias, keyPass);
                kms[0] = smanager;
            }
        }

        sslContext.init(kms,
                tmf != null ? tmf.getTrustManagers() : null, null);
        return sslContext;
    }

    public static SecureChannel createChannel(SSLContext sslContext,
            SocketChannel sc, ChannelSelectorDispatcher st, ChannelListener l,
            ErrorHandler errorHandler) throws Exception {
        SSLEngine engine = sslContext.createSSLEngine();
        engine.setUseClientMode(true);
        return new SecureChannel(st, sc, l, engine, errorHandler);
    }

    public ChannelSelectorDispatcher getDispatcher() {
        return mDispatcher;
    }

    public void establishSSLSession() throws IOException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "establishSSLSession() calling waitForRegisterDone on channel=" + mSocketChannel + " selector=" + mDispatcher.getSelector() + " dispatcher=" + mDispatcher + ")");
        }
        mDispatcher.waitForRegisterDone(mSocketChannel);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "establishSSLSession() after waitForRegisterDone returned channel=" + mSocketChannel + " selector=" + mDispatcher.getSelector() + " dispatcher=" + mDispatcher + ")");
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "establishSSLSession() ============== >>>> calling handshake()");
        }
        handshake();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "establishSSLSession() ============== >>>> handshake() returned");
        }
    }
}