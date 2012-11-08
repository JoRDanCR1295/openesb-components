package com.sun.jbi.systemic.quality.propagation.api.experimental.impl;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.systemic.quality.propagation.api.experimental.MessageExchangeNode;
import com.sun.jbi.systemic.quality.propagation.api.experimental.MutableMessageExchangeNode;

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
