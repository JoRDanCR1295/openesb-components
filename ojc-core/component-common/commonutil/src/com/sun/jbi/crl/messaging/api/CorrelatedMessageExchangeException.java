package com.sun.jbi.crl.messaging.api;

public class CorrelatedMessageExchangeException extends Exception {

	public CorrelatedMessageExchangeException(String message) {
		super(message);
	}
	
	public CorrelatedMessageExchangeException(String message, Throwable th) {
		super(message, th);
	}
}
