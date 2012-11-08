package com.sun.jbi.crl.messaging.api;

public interface MutableMessageExchangeNode extends MessageExchangeNode {

	void addChildExchange(MutableMessageExchangeNode child);
	
	void setParentExchange(MutableMessageExchangeNode parent);
}
