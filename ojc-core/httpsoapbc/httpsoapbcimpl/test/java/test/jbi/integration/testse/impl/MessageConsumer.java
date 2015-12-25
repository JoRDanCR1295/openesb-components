package test.jbi.integration.testse.impl;

import javax.jbi.messaging.MessageExchange;

public interface MessageConsumer {
	
	public void onMessage(MessageExchange ex) throws Exception;

}
