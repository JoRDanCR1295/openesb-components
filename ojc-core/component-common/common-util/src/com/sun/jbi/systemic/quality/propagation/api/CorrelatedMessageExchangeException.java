package com.sun.jbi.systemic.quality.propagation.api;
/**
 * 
 * @author radval
 *
 */
public class CorrelatedMessageExchangeException extends Exception {

	public CorrelatedMessageExchangeException(String message) {
		super(message);
	}
	
	public CorrelatedMessageExchangeException(String message, Throwable th) {
		super(message, th);
	}
}
