package com.sun.jbi.systemic.quality.propagation.api.experimental.impl;

import java.util.ArrayList;
import java.util.List;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.systemic.quality.propagation.api.experimental.MessageExchangeNode;
import com.sun.jbi.systemic.quality.propagation.api.experimental.MutableMessageExchangeNode;

public class MessageExchageNodeImpl implements MutableMessageExchangeNode  {

	private MessageExchangeNode mParentExchange;
	
	private MessageExchange mMessageExchange;
	
	private List<MessageExchangeNode> mChildExchanges = new ArrayList<MessageExchangeNode>();
	
	
	public MessageExchageNodeImpl(MessageExchange exchange) {
		this.mMessageExchange = exchange;
	}
	
	public void addChildExchange(MutableMessageExchangeNode child) {
		child.setParentExchange(this);
		mChildExchanges.add(child);
		
	}
	
	public MessageExchange getMessageExchange() {
		return this.mMessageExchange;
	}
	
	public List<MessageExchangeNode> getChildExchanges() {
		return mChildExchanges;
	}

	public MessageExchangeNode getParentExchange() {
		return mParentExchange;
	}
	
	public void setParentExchange(MutableMessageExchangeNode parent) {
		this.mParentExchange = parent;
	}
}
