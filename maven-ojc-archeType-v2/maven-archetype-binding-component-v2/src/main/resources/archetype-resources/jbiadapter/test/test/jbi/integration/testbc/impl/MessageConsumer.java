#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.testbc.impl;

import javax.jbi.messaging.MessageExchange;

public interface MessageConsumer {
	
	public void onMessage(MessageExchange ex) throws Exception;

}
