package com.sun.jbi.filebc;

import java.util.HashMap;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.filebc.util.FileStreamHandler;

public class PendingExchangeManager {

    private HashMap<String, PendingExchangeHolder> mMap = new HashMap<String, PendingExchangeHolder>();
    private static PendingExchangeManager instance = new PendingExchangeManager();

    public static PendingExchangeManager getInstance() {
        return instance;
    }

    private PendingExchangeManager() {
    }

    synchronized public void saveExchangeHolder(MessageExchange ex, PendingExchangeHolder holder) {
        mMap.put(ex.getExchangeId(), holder);
    }

    synchronized PendingExchangeHolder getExchangeHolder(MessageExchange ex) {
        return mMap.remove(ex.getExchangeId());
    }

    //In future we can add more information that needs to be stored for each exchange
    public static class PendingExchangeHolder {

        FileStreamHandler fileStreamHandler;
    }
}
