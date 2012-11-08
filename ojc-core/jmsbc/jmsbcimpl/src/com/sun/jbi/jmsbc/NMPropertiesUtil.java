package com.sun.jbi.jmsbc;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.NormalizedMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Queue;
import javax.jms.Topic;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSJCAOptions;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOutput;
import com.sun.jbi.jmsbc.jms.ChannelException;
import com.sun.jbi.jmsbc.jms.SendChannel;
import com.sun.jbi.jmsbc.jms.SendChannelJCAImpl;

public class NMPropertiesUtil {
	private static final String TYPE = ".type";
	
	//Common properties
	public static final String INBOUND_MESSAGEID = "org.glassfish.openesb.messaging.inbound.messageid";
	
	
	public static final String MESSAGEID =       "org.glassfish.openesb.messaging.messageid";
	public static final String INBOUND_GROUPID = "org.glassfish.openesb.messaging.groupid";
	public static final String INBOUND_ENDPOINTNAME = "org.glassfish.openesb.exchange.endpointname";
	
	
	//Inbound Only properties
	public static final String INBOUND_OPTIONS = "org.glassfish.openesb.jms.inbound.options";
	public static final String INBOUND_CONNECTIONURL = "org.glassfish.openesb.jms.inbound.connectionurl";
	public static final String INBOUND_USERNAME = "org.glassfish.openesb.jms.inbound.username";
	public static final String INBOUND_MESSAGESELECTOR = "org.glassfish.openesb.jms.inbound.messageselector";
	public static final String INBOUND_DESTINATION = "org.glassfish.openesb.jms.inbound.destination";
	public static final String INBOUND_DESTINATIONTYPE = "org.glassfish.openesb.jms.inbound.destinationtype";
	public static final String INBOUND_FORWARDASATTACHMENT = "org.glassfish.openesb.jms.inbound.forwardasattachment";
	
	//Outbound Only Properties
	public static final String OUTBOUND_SUBSCRIPTIONDURABILITY = "org.glassfish.openesb.jms.outbound.subscriptiondurability";
	public static final String OUTBOUND_SUBSCRIPTIONNAME = "org.glassfish.openesb.jms.outbound.subscriptionname";
	public static final String OUTBOUND_XATRANSACTION = "org.glassfish.openesb.jms.outbound.xatransaction";
	public static final String OUTBOUND_OPTIONS = "org.glassfish.openesb.jms.outbound.options";
	public static final String OUTBOUND_PASSWORD = "org.glassfish.openesb.jms.outbound.password";
	public static final String OUTBOUND_CONNECTIONURL = "org.glassfish.openesb.jms.outbound.connectionurl";
	public static final String OUTBOUND_USERNAME = "org.glassfish.openesb.jms.outbound.username";
	public static final String OUTBOUND_TIMEOUT = "org.glassfish.openesb.jms.outbound.timeOut";
	public static final String OUTBOUND_DELIVERYMODE = "org.glassfish.openesb.jms.outbound.deliverymode";
	public static final String OUTBOUND_MESSAGESELECTOR = "org.glassfish.openesb.jms.outbound.messageselector";
	public static final String OUTBOUND_CLIENTID = "org.glassfish.openesb.jms.outbound.clientid";
	public static final String OUTBOUND_DESTINATIONTYPE = "org.glassfish.openesb.jms.outbound.destinationtype";
	public static final String OUTBOUND_DESTINATION = "org.glassfish.openesb.jms.outbound.destination";
	public static final String OUTBOUND_FORWARDASATTACHMENT = "org.glassfish.openesb.jms.outbound.forwardasattachment";
	
	//Bidirectional properties
	public static final String INOUT_PRIORITY = "org.glassfish.openesb.jms.priority";
	public static final String INOUT_TIMETOLIVE = "org.glassfish.openesb.jms.timetolive";
	public static final String INOUT_MESSAGETYPE = "org.glassfish.openesb.jms.messagetype";
	public static final String INOUT_MESSAGEID = "org.glassfish.openesb.jms.messageid";
	public static final String INOUT_CORRELATIONID = "org.glassfish.openesb.jms.correlationid";
	public static final String INOUT_USERPROPERTIES = "org.glassfish.openesb.jms.userproperties.";
	public static final String INOUT_REPLYTODESTINATION = "org.glassfish.openesb.jms.replytodestination";
	public static final String INOUT_REPLYTODESTINATIONTYPE = "org.glassfish.openesb.jms.replytodestinationtype";
	
	private static final Messages mMessages =
        Messages.getMessages(NMPropertiesUtil.class);
    private static final Logger mLog =
        Messages.getLogger(NMPropertiesUtil.class);
	
	public static void fillNMProperties(Message msg, NormalizedMessage ex,
			JMSOperation operation, JMSAddress address, String dest, boolean isTopic, boolean forwardedAsAttachment) throws JMSException {
		fillNMProperties(msg, ex, operation, address, dest, isTopic?JMSConstants.TOPIC:JMSConstants.QUEUE, forwardedAsAttachment);
	}
	public static void fillNMProperties(Message msg, NormalizedMessage ex,
			JMSOperation operation, JMSAddress address, String dest, String destType, boolean forwardedAsAttachment) throws JMSException {
		
		String value;
		if(msg != null){
			//Fill in JMSMessage properties
			value = ifNotNullTrim(msg.getJMSCorrelationID());
			if(value != null)
				ex.setProperty(INOUT_CORRELATIONID, value);
	
			long expiration = msg.getJMSExpiration();
			ex.setProperty(INOUT_TIMETOLIVE, "" + expiration);
			
			long priority = msg.getJMSPriority();
			ex.setProperty(INOUT_PRIORITY, "" + priority);
	
			
			Destination replyTo = msg.getJMSReplyTo();
			if(replyTo != null){
				if(replyTo instanceof Queue){
					ex.setProperty(INOUT_REPLYTODESTINATION, ((Queue)replyTo).getQueueName());
					ex.setProperty(INOUT_REPLYTODESTINATIONTYPE,JMSConstants.QUEUE);
				}else{
					ex.setProperty(INOUT_REPLYTODESTINATION, ((Topic)replyTo).getTopicName());
					ex.setProperty(INOUT_REPLYTODESTINATIONTYPE, JMSConstants.TOPIC);
				}
			}
			
			value = ifNotNullTrim(msg.getJMSType());
			if(value != null)
				ex.setProperty(INOUT_MESSAGETYPE, value);
	
			//Set JMS User properties
			for(Object property : Collections.list(msg.getPropertyNames())){
				String name = (String)property;
				Object obj = msg.getObjectProperty(name);
				if(obj != null){
					if(obj instanceof Byte){
						mLog.log(Level.WARNING, mMessages.getString("JMSBC-W0741.RedeliveryFailed", "byte"));
						continue;
					}
					ex.setProperty(INOUT_USERPROPERTIES + name, obj.toString());
					String type = null;
					if(obj instanceof Boolean) {
						type = UserPropertyType.BOOLEAN.toString();
					}else if(obj instanceof Short){
						type = UserPropertyType.SHORT.toString();
					}else if(obj instanceof Integer){
						type = UserPropertyType.INT.toString();
					}else if(obj instanceof Long){
						type = UserPropertyType.LONG.toString();
					}else if(obj instanceof Float){
						type = UserPropertyType.FLOAT.toString();
					}else if(obj instanceof Double){
						type = UserPropertyType.DOUBLE.toString();
					}else if(obj instanceof String){
						type = UserPropertyType.STRING.toString();
					}
					if(type != null){
						ex.setProperty(INOUT_USERPROPERTIES + name + TYPE, type);
					}
				}
			}
		}
		
		value = ifNotNullTrim(operation.getMessageSelector());
		if(value != null)
			ex.setProperty(INBOUND_MESSAGESELECTOR, value);
		ex.setProperty(INBOUND_DESTINATION, dest);
		ex.setProperty(INBOUND_DESTINATIONTYPE, destType);

		//JMSAddress properties
		ex.setProperty(INBOUND_CONNECTIONURL, address.getConnectionURL());
		value = ifNotNullTrim(address.getUsername());
		
		if(value != null)
			ex.setProperty(INBOUND_USERNAME, address.getUsername());
		
		if(forwardedAsAttachment){
			ex.setProperty(INBOUND_FORWARDASATTACHMENT, "true");
		}else{
			ex.setProperty(INBOUND_FORWARDASATTACHMENT, "false");
		}
	}
	
	public static void fillJMSMessage(Message msg, NMProperties nm) throws JMSException, NMPropertiesParsingException{
		NMPropertiesImpl nmProperties = (NMPropertiesImpl)nm;
		if(nmProperties.getCorrelationid() != null){
			msg.setJMSCorrelationID(nmProperties.getCorrelationid());
		}
		if(nmProperties.getMessagetype() != null){
			msg.setJMSType(nmProperties.getMessagetype());
		}
		
		try{
			for(UserProperty userProperty : nmProperties.getUserproperties().values()){
				if(userProperty.getValue() == null)
					continue;
				msg.setObjectProperty(userProperty.getName(), userProperty.getObjectValue());
			}
			
		}catch(Throwable t){
			throw new NMPropertiesParsingException(mMessages.getString("JMSBC-E0775.InvalidUserProperty"), t);
		}
	}
	
	public static SendChannel getNewSendChannelIfRequired(SendChannel ch, NMProperties nm, Endpoint ep) throws ChannelException{
		NMPropertiesImpl nmProperties = (NMPropertiesImpl)nm;
		SendChannelJCAImpl channel = (SendChannelJCAImpl)ch;

		if(nmProperties.isNewAddressPropertyAvailable()
				|| nmProperties.isNewOperationPropertyAvailable()){
			JMSAddress address = channel.getJMSAddress();
			JMSOperation operation = channel.getJMSOperation();
			
			//Now set new address properties
			if(nmProperties.isNewAddressPropertyAvailable()){
				address = new JMSAddress();
				if(nmProperties.getConnectionurl() != null){
					address.setConnectionURL(nmProperties.getConnectionurl());
				}
				if(nmProperties.getUsername() != null){
					address.setUsername(nmProperties.getUsername());
				}
				if(nmProperties.getPassword() != null){
					address.setPassword(nmProperties.getPassword());
				}
				if(nmProperties.getOptions()!=null){
					JMSJCAOptions options = address.getJmsjcaOptions();
					if(options == null){
						options = new JMSJCAOptions();
						address.setJmsjcaOptions(options);
					}
					options.setOptions(nmProperties.getOptions());
				}
			}
			
			//set operation properties
			if(nmProperties.isNewOperationPropertyAvailable()){
				operation = operation.getCopy();
				if(nmProperties.getDeliverymode() != null){
					operation.setDeliveryMode(nmProperties.getDeliverymode());
				}
				if(nmProperties.getTimetolive() != -1){
					operation.setTimeToLive(new Long(nmProperties.getTimetolive()));
				}
				if(nmProperties.getPriority() != -1){
					operation.setPriority(new Integer(nmProperties.getPriority()));
				}
				if(nmProperties.getSubscriptiondurability() != null){
					operation.setSubscriptionDurability(nmProperties.getSubscriptiondurability());
					operation.setSubscriptionName(nmProperties.getSubscriptionname());
					operation.setClientID(nmProperties.getClientid());
				}
				if(nmProperties.getMessageselector() != null){
					operation.setMessageSelector(nmProperties.getMessageselector());
				}
			}

			channel = new SendChannelJCAImpl();
			channel.initialize(address, operation);
			channel.open();
			channel.start();
		}
		return channel;
	}

	//Get the message exchange specific NMProperties
	public static SynchronousReadNMProperties getSynchronousReadNMProperties(NormalizedMessage ex) throws NMPropertiesParsingException{
		return new SynchronousReadNMPropertiesImpl(ex);
	}
	public static OutBoundInOnlyNMProperties getOutBoundInOnlyNMProperties(NormalizedMessage ex) throws NMPropertiesParsingException{
		return new OutBoundInOnlyNMPropertiesImpl(ex);
	}
	public static InboundInOutNMProperties getInboundInOutNMProperties(NormalizedMessage ex) throws NMPropertiesParsingException{
		return new InboundInOutNMPropertiesImpl(ex);
	}
	public static OutBoundInOutNMProperties getOutBoundInOutNMProperties(NormalizedMessage ex) throws NMPropertiesParsingException{
		return new OutBoundInOutNMPropertiesImpl(ex);
	}
	
	private static String ifNotNullTrim(Object str){
		String value = (String)str;
		if(value != null && !(value = value.trim()).equals("")){
			return value;
		}
		
		return null;
	}
	
	public interface NMProperties{
		//Tag interface
	}
	
	public interface SynchronousReadNMProperties extends NMProperties{
		public String getDestination();
		public String getDestinationtype();
		public String getConnectionurl();
		public String getOptions() ;
		public String getPassword() ;
		public String getUsername() ;
		public String getXatransaction();
		public String getMessageselector();
		public long getTimeOut() ;
		public String getClientid() ;
		public String getSubscriptionname();
		public String getForwardAsAttachment();
	}
	
	private static class SynchronousReadNMPropertiesImpl extends NMPropertiesImpl implements SynchronousReadNMProperties{
		private SynchronousReadNMPropertiesImpl(NormalizedMessage ex) throws NMPropertiesParsingException{
			setDestination(ex);
			setDestinationtype(ex);
			setConnectionurl(ex);
			setOptions(ex);
			setPassword(ex);
			setUsername(ex);
			setXatransaction(ex);
			setMessageselector(ex);
			setTimeOut(ex);
			setClientid(ex);
			setSubscriptionname(ex);
			addUserproperties(ex);
			setSubscriptiondurability(ex);
			setForwardAsAttachment(ex);
			validate();
		}
	}

	public interface InboundInOutNMProperties extends NMProperties{
		public String getConnectionurl();
		public String getCorrelationid();
		public void setCorrelationid(String id);
		public String getDeliverymode(); 
		public String getMessagetype();
		public String getOptions() ;
		public String getPassword() ;
		public int getPriority() ;
		public String getReplytodestination() ;
		public String getReplytodestinationtype() ;
		public long getTimetolive() ;
		public String getUsername() ;
		public String getXatransaction();
	}
	
	private static class InboundInOutNMPropertiesImpl extends NMPropertiesImpl implements InboundInOutNMProperties{
		private InboundInOutNMPropertiesImpl(NormalizedMessage ex) throws NMPropertiesParsingException{
			setConnectionurl(ex);
			setCorrelationid(ex);
			setDeliverymode(ex);
			setMessagetype(ex);
			setOptions(ex);
			setPassword(ex);
			setPriority(ex);
			setReplytodestination(ex);
			setReplytodestinationtype(ex);
			setTimetolive(ex);
			setUsername(ex);
			addUserproperties(ex);
			setXatransaction(ex);
			validate();
		}

		public void setCorrelationid(String id) {
		    this.correlationid=id;
		}
	}

	public interface OutBoundInOutNMProperties extends NMProperties{
		public String getConnectionurl();
		public String getCorrelationid();
		public String getDeliverymode(); 
		public String getDestination();
		public String getDestinationtype();
		public String getMessagetype();
		public String getOptions() ;
		public String getPassword() ;
		public int getPriority() ;
		public String getReplytodestination() ;
		public String getReplytodestinationtype() ;
		public long getTimetolive() ;
		public String getUsername() ;
		public long getTimeOut() ;
		public String getForwardAsAttachment();
	}
	
	private static class OutBoundInOutNMPropertiesImpl extends NMPropertiesImpl implements OutBoundInOutNMProperties{
		private OutBoundInOutNMPropertiesImpl(NormalizedMessage ex) throws NMPropertiesParsingException{
			setDestination(ex);
			setDestinationtype(ex);
			setTimetolive(ex);
			setPriority(ex);
			setUsername(ex);
			setReplytodestination(ex);
			setReplytodestinationtype(ex);
			setConnectionurl(ex);
			setCorrelationid(ex);
			setMessagetype(ex);
			setTimeOut(ex);
			setDeliverymode(ex);
			setPassword(ex);
			setOptions(ex);
			addUserproperties(ex);
			setForwardAsAttachment(ex);
			validate();
		}
	}
	
	public interface OutBoundInOnlyNMProperties extends NMProperties{
		public String getConnectionurl();
		public String getCorrelationid();
		public String getDeliverymode(); 
		public String getDestination();
		public String getDestinationtype();
		public String getMessagetype();
		public String getOptions() ;
		public String getPassword() ;
		public int getPriority() ;
		public String getReplytodestination() ;
		public String getReplytodestinationtype() ;
		public long getTimetolive() ;
		public String getUsername() ;
		public String getXatransaction();
	}
	
	private static class OutBoundInOnlyNMPropertiesImpl extends NMPropertiesImpl implements OutBoundInOnlyNMProperties {
		private OutBoundInOnlyNMPropertiesImpl(NormalizedMessage ex) throws NMPropertiesParsingException{
			setDestination(ex);
			setDestinationtype(ex);
			setTimetolive(ex);
			setPriority(ex);
			setUsername(ex);
			setReplytodestination(ex);
			setReplytodestinationtype(ex);
			setConnectionurl(ex);
			setCorrelationid(ex);
			setMessagetype(ex);
			setXatransaction(ex);
			setDeliverymode(ex);
			setPassword(ex);
			setOptions(ex);
			addUserproperties(ex);
			validate();
		}

	}
	private static class NMPropertiesImpl{
		private String destination;
		private String destinationtype;
		private String clientid;
		private String messageselector;
		private String deliverymode;
		private long timetolive = -1;
		private int priority = -1;
		private long timeOut = -1;
		private String username;
		private String replytodestinationtype;
		private String replytodestination;
		private String connectionurl;
		private String password;
		private String options;
		protected String correlationid;
		private String xatransaction;
		private String subscriptionname;
		private String messagetype;
		private String subscriptiondurability;
		private boolean newJmsAddressPropertiesAvailable = false;
		private boolean newJmsOpeartionPropertiesAvailable = false;
		private String forwardAsAttachment;
		
		private NMPropertiesImpl(){
		}
		
		//private Map<String, String> userproperties = new HashMap<String, String>();
		private HashMap<String, UserProperty> userproperties = new HashMap<String, UserProperty>();
		
		public String getDestination() {
			return destination;
		}
		public void setDestination(NormalizedMessage ex) {
			this.destination = ifNotNullTrim(ex.getProperty(OUTBOUND_DESTINATION));
		}
		public String getDestinationtype() {
			return destinationtype;
		}
		public void setDestinationtype(NormalizedMessage ex) throws NMPropertiesParsingException {
			this.destinationtype = ifNotNullTrim(ex.getProperty(OUTBOUND_DESTINATIONTYPE));
			isValidDestinationType(OUTBOUND_DESTINATIONTYPE, this.destinationtype);
		}
		public String getClientid() {
			return clientid;
		}
		public void setClientid(NormalizedMessage ex) {
			this.clientid = ifNotNullTrim(ex.getProperty(OUTBOUND_CLIENTID));
			turnOnNewJmsOpeartionPropertiesAvailable(this.clientid);
			return;
		}
		public String getMessageselector() {
			return messageselector;
		}
		public void setMessageselector(NormalizedMessage ex) {
			this.messageselector = ifNotNullTrim(ex.getProperty(OUTBOUND_MESSAGESELECTOR));
			turnOnNewJmsOpeartionPropertiesAvailable(this.messageselector);
		}
		public String getDeliverymode() {
			return deliverymode;
		}
		public void setDeliverymode(NormalizedMessage ex) throws NMPropertiesParsingException{
			this.deliverymode = ifNotNullTrim(ex.getProperty(OUTBOUND_DELIVERYMODE));
			if(this.deliverymode == null)
				return;
			
			if (!this.deliverymode.equals(JMSConstants.DELIVERYMODE_PERSISTENT)
					&& !this.deliverymode
							.equals(JMSConstants.DELIVERYMODE_NON_PERSISTENT)) {
				String msg = mMessages.getString(
						"JMSBC-E0765.InvalidNormalizedProperty", new Object[] {
								OUTBOUND_DELIVERYMODE, this.deliverymode });
				throw new NMPropertiesParsingException(msg);
			}
			turnOnNewJmsOpeartionPropertiesAvailable(this.deliverymode);
		}
		public long getTimetolive() {
			return timetolive;
		}
		public void setTimetolive(NormalizedMessage ex) throws NMPropertiesParsingException{
		    if (!checkInboundNormalizedMessage(ex)) {
			this.timetolive = getNumberValue(ex, INOUT_TIMETOLIVE);
			turnOnNewJmsOpeartionPropertiesAvailable(this.timetolive);
		    }
		}

		public int getPriority() {
			return priority;
		}
		public void setPriority(NormalizedMessage ex) throws NMPropertiesParsingException {
		    if (!checkInboundNormalizedMessage(ex)) {
			this.priority = (int) getNumberValue(ex, INOUT_PRIORITY);
			turnOnNewJmsOpeartionPropertiesAvailable(this.priority);
		    }

		}
		private boolean checkInboundNormalizedMessage(NormalizedMessage ex) {
		    return ex.getProperty(INBOUND_MESSAGEID) != null;
		}
		
		public long getTimeOut() {
			return timeOut;
		}
		public void setTimeOut(NormalizedMessage ex) throws NMPropertiesParsingException{
			this.timeOut= getNumberValue(ex, OUTBOUND_TIMEOUT);
		}
		public String getUsername() {
			return username;
		}
		public void setUsername(NormalizedMessage ex) {
			this.username = ifNotNullTrim(ex.getProperty(OUTBOUND_USERNAME));
			turnOnNewJmsAddressPropertiesAvailable(this.username);
		}
		public String getReplytodestinationtype() {
			return replytodestinationtype;
		}
		public void setReplytodestinationtype(NormalizedMessage ex) throws NMPropertiesParsingException {
			this.replytodestinationtype = ifNotNullTrim(ex.getProperty(INOUT_REPLYTODESTINATIONTYPE));
			isValidDestinationType(INOUT_REPLYTODESTINATIONTYPE, this.replytodestinationtype);
		}
		public String getReplytodestination() {
			return replytodestination;
		}
		public void setReplytodestination(NormalizedMessage ex) {
			this.replytodestination = ifNotNullTrim(ex.getProperty(INOUT_REPLYTODESTINATION));
		}
		public String getConnectionurl() {
			return connectionurl;
		}
		public void setConnectionurl(NormalizedMessage ex) {
			this.connectionurl = ifNotNullTrim(ex.getProperty(OUTBOUND_CONNECTIONURL));
			turnOnNewJmsAddressPropertiesAvailable(this.connectionurl);
		}
		public String getPassword() {
			return password;
		}
		public void setPassword(NormalizedMessage ex) {
			this.password = ifNotNullTrim(ex.getProperty(OUTBOUND_PASSWORD));
			turnOnNewJmsAddressPropertiesAvailable(this.password);
		}
		public String getOptions() {
			return options;
		}
		public void setOptions(NormalizedMessage ex) {
			this.options = ifNotNullTrim(ex.getProperty(OUTBOUND_OPTIONS));
			turnOnNewJmsAddressPropertiesAvailable(this.options);
		}
		public HashMap<String, UserProperty> getUserproperties() {
			return userproperties;
		}
		public void addUserproperties(NormalizedMessage ex) throws NMPropertiesParsingException {
			try{
				for(Object obj : ex.getPropertyNames()){
					String key = (String)obj;
					boolean isType = false;
					if(key.startsWith(INOUT_USERPROPERTIES)){
                                            String value = null;
                                            boolean isDouble = false;
                                            boolean isString = false;
                                            boolean isBoolean = false;
                                            if (ex.getProperty(key) instanceof String){
                                                value = (String)ex.getProperty(key);
                                                isString = true;
                                            } else if (ex.getProperty(key) instanceof Boolean){
                                                value = Boolean.toString((Boolean)ex.getProperty(key));
                                                isBoolean = true;
                                            } else if (ex.getProperty(key) instanceof Double){
                                                value = (Long.toString(((Double)ex.getProperty(key)).longValue()));
                                                isDouble = true;
                                            } else
                                                throw new NMPropertiesParsingException("Unsupported type of user property");

						if(value!=null && (value = value.trim()).length() > 0){
							if(key.endsWith(TYPE)){
								key = key.substring(0, key.length() - 5);
								isType = true;
							}
							int i = key.lastIndexOf('.');
							String name = null;
							if(i != -1 && i < (key.length()-1)){
								name = key.substring(i+1);
							}
							if(name != null){
								UserProperty userProperty = userproperties.get(name);
								if(userProperty == null){
                                                                        if (isDouble)
                                                                            userProperty = new UserProperty(name,UserPropertyType.DOUBLE);
                                                                        else if (isBoolean)
                                                                            userProperty = new UserProperty(name,UserPropertyType.BOOLEAN);
                                                                        else if(isString)
									userProperty = new UserProperty(name);
									userproperties.put(name, userProperty);
								}
								if(isType){
									userProperty.setType(value);
								}else{
									userProperty.setValue(value);
								}
							}
						}
					}
				}
			}catch(Throwable t){
				throw new NMPropertiesParsingException(mMessages.getString("JMSBC-E0775.InvalidUserProperty"), t);
			}
		}
		public String getCorrelationid() {
			return correlationid;
		}
		public void setCorrelationid(NormalizedMessage ex) {
			this.correlationid = ifNotNullTrim(ex.getProperty(INOUT_CORRELATIONID));
		}
		public String getXatransaction() {
			return xatransaction;
		}
		public void setXatransaction(NormalizedMessage ex) throws NMPropertiesParsingException{
			this.xatransaction = ifNotNullTrim(ex.getProperty(OUTBOUND_XATRANSACTION));
			if(this.xatransaction == null)
				return;
			
			if (!this.xatransaction.equals(JMSConstants.TRANSACTION_NONE)
					&& !this.xatransaction
							.equals(JMSConstants.TRANSACTION_XA)) {
				String msg = mMessages.getString(
						"JMSBC-E0765.InvalidNormalizedProperty", new Object[] {
								OUTBOUND_XATRANSACTION, this.xatransaction });
				throw new NMPropertiesParsingException(msg);
			}
		}
		public String getSubscriptionname() {
			return subscriptionname;
		}
		public void setSubscriptionname(NormalizedMessage ex) {
			this.subscriptionname = ifNotNullTrim(ex.getProperty(OUTBOUND_SUBSCRIPTIONNAME));
			turnOnNewJmsOpeartionPropertiesAvailable(this.subscriptionname);
		}
		public String getMessagetype() {
			return messagetype;
		}
		public void setMessagetype(NormalizedMessage ex) {
			this.messagetype = ifNotNullTrim(ex.getProperty(INOUT_MESSAGETYPE));
		}

		private void turnOnNewJmsAddressPropertiesAvailable(String str) {
			if (str != null)
				newJmsAddressPropertiesAvailable = true;
		}

		private void turnOnNewJmsOpeartionPropertiesAvailable(String str) {
			if (str != null)
				newJmsOpeartionPropertiesAvailable= true;
		}
		private void turnOnNewJmsOpeartionPropertiesAvailable(long value) {
			if (value != -1)
				newJmsOpeartionPropertiesAvailable = true;
		}

		public long getNumberValue(NormalizedMessage ex, String property)
				throws NMPropertiesParsingException {
			String value = ifNotNullTrim(ex.getProperty(property));
			long result = -1;
			if (value == null)
				return result;
			try {
				result = Long.parseLong(value);
			} catch (Throwable t) {
				String msg = mMessages.getString(
						"JMSBC-E0765.InvalidNormalizedProperty", new Object[] {
								property, value });
				throw new NMPropertiesParsingException(msg, t);
			}
			return result;
		}
		public String getSubscriptiondurability() {
			return subscriptiondurability;
		}
		public void setSubscriptiondurability(NormalizedMessage ex) throws NMPropertiesParsingException{
			this.subscriptiondurability = ifNotNullTrim(ex.getProperty(OUTBOUND_SUBSCRIPTIONDURABILITY));
			if (this.subscriptiondurability != null
					&& !this.subscriptiondurability
							.equalsIgnoreCase(JMSConstants.DURABLE)
					&& !this.subscriptiondurability
							.equalsIgnoreCase(JMSConstants.NON_DURABLE)) {
				String msg = mMessages.getString(
						"JMSBC-E0765.InvalidNormalizedProperty", new Object[] {
								OUTBOUND_SUBSCRIPTIONDURABILITY,
								this.subscriptiondurability });
				throw new NMPropertiesParsingException(msg);
			}
			turnOnNewJmsOpeartionPropertiesAvailable(this.subscriptiondurability);
		}
		
		public boolean isNewOperationPropertyAvailable(){
			return newJmsOpeartionPropertiesAvailable;
		}
		public boolean isNewAddressPropertyAvailable(){
			return newJmsAddressPropertiesAvailable;
		}
		
		private void isValidDestinationType(String property, String destType) throws NMPropertiesParsingException{
			if(destType == null)
				return;
			
			if(!destType.equals(JMSConstants.TOPIC) && !destType.equals(JMSConstants.QUEUE))
				throw new NMPropertiesParsingException(mMessages.getString(
						"JMSBC-E0765.InvalidNormalizedProperty", new Object[] {
								property,
								destType}));
		}
		
		public void validate() throws NMPropertiesParsingException{
			
			String msg = "";
//			if ((getDestination() != null || getDestinationtype() != null)
//					&& (getDestination() == null || getDestinationtype() == null)
//
//			) {
//				msg += mMessages.getString("JMSBC-E0766.InvalidNormalizedProperty") + "\n";
//			}

			if ((getReplytodestination() != null || getReplytodestinationtype() != null)
					&& (getReplytodestination() == null || getReplytodestinationtype() == null)

			) {
				msg += mMessages.getString("JMSBC-E0767.InvalidNormalizedProperty") + "\n";
			}

			if ((getSubscriptiondurability() != null
					|| getSubscriptionname() != null)
					&& (getSubscriptiondurability() == null
							|| getSubscriptionname() == null || getClientid() == null)

			) {
				msg += mMessages.getString("JMSBC-E0768.InvalidNormalizedProperty") + "\n";
			}
			
			if(!msg.equals("")){
				throw new NMPropertiesParsingException(msg);
			}
		}
		public String getForwardAsAttachment() {
			return forwardAsAttachment;
		}
		
		public void setForwardAsAttachment(NormalizedMessage ex) {
			String str = ifNotNullTrim(ex.getProperty(OUTBOUND_FORWARDASATTACHMENT));
			if(str != null){
				this.forwardAsAttachment = Boolean.toString(Boolean.parseBoolean(str));
			}
		}
		
	}
	
	public static class NMPropertiesParsingException extends Exception{
	    public NMPropertiesParsingException() {
	        super();
	    }

	    public NMPropertiesParsingException(String message) {
	        super(message);
	    }

	    public NMPropertiesParsingException(String message, Throwable cause) {
	        super(message, cause);
	    }

	    public NMPropertiesParsingException(Throwable cause) {
	        super(cause);
	    }
	}
	
	private static class UserProperty{
		private UserPropertyType type;
		private String value;
		private String name;
		private Object valueObject;
		
		public UserProperty(String name){
			this.name = name;
			this.type = UserPropertyType.STRING;
		}
		public UserProperty(String name, UserPropertyType type){
			this.name = name;
			this.type = type;
		}
		
		public UserPropertyType getType() {
			return type;
		}

		public void setType(String type) {
			this.type = UserPropertyType.parseUserPropertyType(type);
		}
		
		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}
		
		public String getName() {
			return name;
		}

		@Override
		public boolean equals(Object obj) {
			return name.equals(obj);
		}

		@Override
		public int hashCode() {
			return name.hashCode();
		}

		@Override
		public String toString() {
			return "Name=" + name + ",Value=" + value + ",Type=" + type.toString();
		}
		
		public Object getObjectValue(){
			Object result = null;
			if(type == UserPropertyType.STRING){
				result = value;
			}else if(type == UserPropertyType.BOOLEAN){
				result = new Boolean(value);
			}else if(type == UserPropertyType.DOUBLE){
				result = new Double(value);
			}else if(type == UserPropertyType.FLOAT){
				result = new Float(value);
			}else if(type == UserPropertyType.INT){
				result = new Integer(value);
			}else if(type == UserPropertyType.LONG){
				result = new Long(value);
			}else if(type == UserPropertyType.SHORT){
				result = new Short(value); 
			}
			
			return result;
		}
	}
	
	enum UserPropertyType{
		BOOLEAN("boolean"),
		DOUBLE("double"),
		FLOAT("float"),
		INT("int"),
		LONG("long"),
		SHORT("short"),
		STRING("string");
	
		String type;
		UserPropertyType(String type){
			this.type = type;
		}
		
		public String toString(){
			return type;
		}
		
		public static UserPropertyType parseUserPropertyType(String value){
			if(value == null)
				throw new RuntimeException("Invalid UserPropertyType NULL");
			
			value = value.trim();
			if(value.equalsIgnoreCase(BOOLEAN.toString())){
				return BOOLEAN;
			}else if(value.equalsIgnoreCase(DOUBLE.toString())){
				return DOUBLE;
			}else if(value.equalsIgnoreCase(FLOAT.toString())){
				return FLOAT;
			}else if(value.equalsIgnoreCase(INT.toString())){
				return INT;
			}else if(value.equalsIgnoreCase(LONG.toString())){
				return LONG;
			}else if(value.equalsIgnoreCase(SHORT.toString())){
				return SHORT;
			}else if(value.equalsIgnoreCase(STRING.toString())){
				return STRING;
			}else {
				throw new RuntimeException("Invalid UserPropertyType " + value);
			}
		}
	}
}
