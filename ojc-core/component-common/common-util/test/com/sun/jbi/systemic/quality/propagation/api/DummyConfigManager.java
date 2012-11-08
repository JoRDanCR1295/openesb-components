/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.api;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;

/**
 *  crude implementation of ConfigManager for testing.
 *  
 * @author radval
 */
public class DummyConfigManager implements ConfigManager {
        
        private ConfigManager.TRANSACTIONTYPE mTransactionType;
        
        private ConfigManager.SECURITYTYPE mSecurityType;
        
        private Map<MessageExchange, List<ExchangeToNewTransactionMapping>> mExchangeToTransactioMap =
        		new HashMap<MessageExchange, List<ExchangeToNewTransactionMapping>>();
        
        public DummyConfigManager(ConfigManager.TRANSACTIONTYPE tType,
                                  ConfigManager.SECURITYTYPE sType) {
            
            this.mTransactionType = tType;
            this.mSecurityType = sType;
            
        }
        
        public TRANSACTIONTYPE getTransactionType(MessageExchange to) {
            return this.mTransactionType;
        }

        public Object createNewTransaction(MessageExchange childExchange) {
        	//let this be dummy transaction object
            Object transaction = new Object();
            
            ExchangeToNewTransactionMapping mapping = new ExchangeToNewTransactionMapping(childExchange, transaction);
            
            List<ExchangeToNewTransactionMapping> list = mExchangeToTransactioMap.get(childExchange);
            if(list == null) {
            	list = new ArrayList<ExchangeToNewTransactionMapping>();
            	mExchangeToTransactioMap.put(childExchange, list);
            }
            
            list.add(mapping);
            
            
            
            return transaction;
        }

        public Object getTransaction(MessageExchange childExchange) {
            
        	  if(childExchange == null) {
        		  return null;
        	  }
        	  
        	  Object transaction = null;
        	  
        	  List<ExchangeToNewTransactionMapping> list = mExchangeToTransactioMap.get(childExchange);
              
        	  if(list != null) {
        		  Iterator<ExchangeToNewTransactionMapping> it = list.iterator();
        		  while(it.hasNext()) {
        			  ExchangeToNewTransactionMapping mapping = it.next();
        			  if(childExchange.equals(mapping.getChildExchange())) {
        				  transaction = mapping.getTransaction();
        				  break;
        			  }
        		  }
        	  }
        	  
        	  return transaction;
        }

		public SECURITYTYPE getSecurityType(MessageExchange childExchange) {
			return this.mSecurityType;
		}
		
		
		class ExchangeToNewTransactionMapping {

			private MessageExchange mChildExchange;
			private Object mTransaction;
			
			ExchangeToNewTransactionMapping(MessageExchange childExchange,
											Object transaction) {

				this.mChildExchange = childExchange;
				this.mTransaction = transaction;
			}
			
			public MessageExchange getChildExchange() {
				return this.mChildExchange;
			}
			
			public Object getTransaction(){
				return this.mTransaction;
			}

			
		}
    }
