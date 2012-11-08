package com.sun.jbi.systemic.quality.propagation.api.experimental;

import java.util.List;

import javax.jbi.messaging.MessageExchange;

public interface MessageExchangeNode {

	public static final String MESSAGE_EXCHANGE_PARENT_NODE = "javax.jbi.messageexchange.parent.node";
	
	MessageExchange getMessageExchange();
	
	MessageExchangeNode getParentExchange();
	
	List<MessageExchangeNode> getChildExchanges();
	
	
}
