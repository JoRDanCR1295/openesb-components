package com.sun.jbi.common.qos.messaging.filter;

import com.sun.jbi.common.qos.I18n;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 *
 * @author VFrolkin
 */
public class FilterChainImpl implements FilterChain {

    private LinkedList<ExchangeFilter> chain;

    public FilterChainImpl() {
        chain = new LinkedList<ExchangeFilter>();
    }

    public boolean remove(ExchangeFilter e) {
        return chain.remove(e);
    }

    public void addLast(ExchangeFilter e) {
        chain.addLast(e);
    }

    public void addFirst(ExchangeFilter e) {
        chain.addFirst(e);
    }

    public MessageExchange processOutgoingExchange(MessageExchange msg, Object... params) throws MessagingException {
        MessageExchange ex = null;
        for (ExchangeFilter filter : chain) {
            ex = filter.processOutgoingExchange(msg, params);
            if (ex == null)
                break;
        }
        return ex;
    }

    public MessageExchange processIncomingExchange(MessageExchange msg, Object... params) throws MessagingException {
        MessageExchange ex = null;

        for (int i = chain.size() - 1; i >= 0; i--)
            if ((ex = chain.get(i).processIncomingExchange(msg, params)) == null)
                break;

//      Commented until 1.6
//        Iterator<ExchangeFilter> iterator = chain.descendingIterator();
//        while (iterator.hasNext())
//            if ((ex = iterator.next().processIncomingExchange(msg, params)) == null)
//                break;
        return ex;
    }

    public void close() throws MessagingException {
        MessagingException ex = null;
        for (ExchangeFilter filter : chain)
            try {
                filter.close();
            } catch (Exception e) {
                // catch to make sure DC has a chance to close
                String msg = I18n.loc(
                        "QOS-6076: {0} failed to close: {1}",
                        filter.getClass().getSimpleName(), e.getMessage());
                Logger.getAnonymousLogger().log(Level.WARNING, msg, e); //TEMP
                ex = new MessagingException(msg, e);
            }
        if (ex != null)
            throw ex;
    }

    public List<ExchangeFilter> getFilterList() {
        return chain;
    }
}
