package com.sun.jbi.crl.messaging.api;

import java.util.List;

import javax.jbi.messaging.MessageExchange;

public interface MessageExchangeNode {

	public static final String MESSAGE_EXCHANGE_PARENT_NODE = "javax.jbi.messageexchange.parent.node";
	
	MessageExchange getMessageExchange();
	
	MessageExchangeNode getParentExchange();
	
	List<MessageExchangeNode> getChildExchanges();
	
	
}
