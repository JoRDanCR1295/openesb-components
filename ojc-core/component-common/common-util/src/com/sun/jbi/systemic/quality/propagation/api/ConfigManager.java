package com.sun.jbi.systemic.quality.propagation.api;

import javax.jbi.messaging.MessageExchange;
/**
 * 
 * 
 * ConfigManager is a class which provides manipulation to
 * transaction and security propagation after the 
 * {@link #ParentChildExchangeCorrelator.assignChildExchange()} is called.
 * 
 * It also transaction and security types to be returned from
 * here which are then used to change the behaviour of these
 * two propagation.
 * 
 * ConfigManager will later be automatically provided
 * using ConfigManagerFactory which allows declarative
 * transaction/security propagation. Some components (like BPELSE)
 * may still choose to have an adapter like implementation
 * which can use declarative as well as custom implementation
 * of how to influence these propagation.
 * 
 * @author radval
 *
 */
public interface ConfigManager {

	/**
	 * Get the transaction type constant used in deciding how to propagation
	 * transaction from parentExchange to childExchange
	 * @param parentExchange
	 * @param childExchange
	 * @return TRANSACTIONTYPE
	 */
	TRANSACTIONTYPE getTransactionType(MessageExchange childExchange);
	
	/**
	 * Get the security type constant used in deciding how to propagation
	 * security from parentExchange to childExchange
	 * @param parentExchange
	 * @param childExchange
	 * @return SECURITYTYPE
	 */
	SECURITYTYPE getSecurityType(MessageExchange childExchange);
	
	/**
	 * Create a new transaction when transaction type
	 * returned from {@link #getTransactionType(MessageExchange, MessageExchange)}
	 * is REQUIRES_NEW
	 * @param parentExchange
	 * @param childExchange
	 * @return JTA Transaction object
	 */
	Object createNewTransaction(MessageExchange childExchange);
	
	/**
	 * Get the transaction which is used on child exchange.
	 * @param parentExchange
	 * @param childExchange
	 * @return JTA Transaction object.
	 */
	Object getTransaction(MessageExchange childExchange);
	
	enum TRANSACTIONTYPE {
		
		/**
		 * REQUIRED is used when propagating transaction.
		 *   
		 */
		//REQUIRED("REQUIRED"),
		/**
		 * REQUIRES_NEW is used when propagating transaction to
		 * child exchange. Always start a new transaction and set it
		 * on the child exchange.
		 * 
		 */
		REQUIRES_NEW("REQUIRES_NEW"),
		//NOT_SUPPORTED("NOT_SUPPORTED"), 
		//BEGIN_OR_JOIN("BEGIN_OR_JOIN"),
		/**
		 * ALWAYS_JOIN is used when propagating transaction to child exchange. 
		 * Always expects a transaction to be in progress when 
		 * a new child exchange is created. 
		 * if there is no transaction an exception is thrown.  
		 */
		//ALWAYS_JOIN("ALWAYS_JOIN"),
		/**
		 * USE_PARENT is used when propagating transaction to child exchange.
		 * When used, parent exchange's transaction object will be set
		 * on child exchange. if parent exchange does not have transaction
		 * an exception will be thrown. 
		 */
		JOIN_PARENT("JOIN_PARENT"),
		/**
		 * NEVER is used when propagation transaction to child exchange.
		 * When used, no transaction will be set on child exchange.
		 */
		NEVER("NEVER");
		
		private final String mType;
		
		private TRANSACTIONTYPE(String type) {
			this.mType = type;
		}
		
		public String toString() {
			return this.mType;
		}
		
	}
	
	enum SECURITYTYPE {
		
		/**
		 * ALWAYS is used when you always wants to propagate Security context
		 * from parent exchange to child exchange.
		 * 
		 */
		ALWAYS("ALWAYS"),
		/**
		 * NEVER is used when you do not want to propagate Security context
		 * from parent exchange to child exchange.
		 */
		NEVER("NEVER");
		
		private final String mType;
		
		private SECURITYTYPE(String type) {
			this.mType = type;
		}
		
		public String toString() {
			return this.mType;
		}
	}
}
