package com.sun.jbi.systemic.quality.propagation.api.experimental;

public interface MutableMessageExchangeNode extends MessageExchangeNode {

	void addChildExchange(MutableMessageExchangeNode child);
	
	void setParentExchange(MutableMessageExchangeNode parent);
}
