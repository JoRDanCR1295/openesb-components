package test.jbi.integration.testbc.impl;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

public class MessageProcessor implements Runnable {
	private static final Logger mLog = Logger.getLogger(MessageProcessor.class.getName());	
	private ComponentContext mComponentContext;
	private DeliveryChannel mDeliveryChannel;
	private boolean stop;
	private Hashtable<String, Object> mConsumers = new Hashtable<String, Object>();
	
	
	MessageProcessor(ComponentContext ctx) throws MessagingException{
		mComponentContext = ctx;
		mDeliveryChannel = mComponentContext.getDeliveryChannel();
	}

	public void run() {
		
		while(!stop){
			try {
				final MessageExchange ex = mDeliveryChannel.accept(1000 * 60);
				if(ex == null)
					continue;
				
				JbiHelper.execute(new Runnable() {
					public void run() {
						try{
							processMessage(ex);
						}catch(Throwable t){
							mLog.log(Level.INFO, "Error processing message.", t);
						}
					}
				});
				
			} catch (MessagingException e) {
				mLog.log(Level.INFO, "Error accepting message exchanges from delivery channel.", e);
			}
		}

	}
	
	private void processMessage(MessageExchange ex) throws Exception{
		ServiceEndpoint ep = ex.getEndpoint();
		String key = getKey(ep.getServiceName(), ep.getEndpointName());
		Object obj = mConsumers.get(key);
		if(obj == null){
			mLog.log(Level.INFO,
					"Message Exchange available for the end point " + key
						+ " not active anymore. Drop the message exchange");
			return;
		}
		
		if(obj instanceof LinkedList){
			synchronized (obj) {
				((LinkedList<MessageExchange>)obj).addLast(ex);
			}
		}else{
			((MessageConsumer)obj).onMessage(ex);
		}
	}

	synchronized public void stop(){
		stop = true;
	}
	
	public void start(){
		stop = false;
		JbiHelper.execute(this);
	}
	
	public void registerEndPoint(QName serviceName, String endpointName){
		registerEndPoint(serviceName, endpointName, new LinkedList<MessageExchange>());
	}
	
	public void registerEndPoint(QName serviceName, String endpointName, MessageConsumer c){
		registerEndPoint(serviceName, endpointName, c);
	}

	private void registerEndPoint(QName serviceName, String endpointName, Object obj){
		mConsumers.put(getKey(serviceName, endpointName), obj);
	}

	private String getKey(QName serviceName, String endpointName) {
		return serviceName.toString() + "-" + endpointName;
	}
	
	public Object unregisterEndPoint(QName serviceName, String endpointName){
		Object result = mConsumers.remove(getKey(serviceName, endpointName));
		if(result instanceof LinkedList){
			synchronized (result) {
				((LinkedList<MessageExchange>)result).clear();
			}
		}
		return result;
	}
	
	public MessageExchange getNextMessage(QName serviceName, String endpointName){
		Object obj = mConsumers.get(getKey(serviceName, endpointName));
		if(obj instanceof LinkedList){
			synchronized (obj) {
				LinkedList<MessageExchange> l = (LinkedList<MessageExchange>)obj;
				if(l.size() != 0){
					return l.removeFirst();
				}
			}
		}
		
		return null;
	}
	
}
