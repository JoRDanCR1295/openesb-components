package com.sun.jbi.crl.messaging.api.impl;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.messaging.api.MessageExchangeNode;
import com.sun.jbi.crl.messaging.api.MutableMessageExchangeNode;

public class MessageExchangeNodeFactory {

	private static MessageExchangeNodeFactory mInstance;
	
	public static synchronized MessageExchangeNodeFactory getInstance() {
		
		if(mInstance == null) {
			mInstance = new MessageExchangeNodeFactory();
		}
		
		return mInstance;
	}
	
	
	public MutableMessageExchangeNode createMessageExchangeNode(MessageExchange parent) {
		synchronized (parent) {
			return new MessageExchageNodeImpl(parent);
		}
		
	}
	
	
}
