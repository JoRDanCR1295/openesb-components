package com.sun.jbi.jmsbc.mbeans;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.management.MBeanException;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.Endpoint;
import com.sun.jbi.jmsbc.ServiceUnit;
import com.sun.jbi.jmsbc.Endpoint.EndpointState;
import com.sun.jbi.jmsbc.Endpoint.EndpointType;

public class JMSBCManagement implements JMSBCManagementMBean {
	
    private static final Messages mMessages =
        Messages.getMessages(JMSBCManagement.class);

	private Collection<ServiceUnit> mServiceUnits;

	public JMSBCManagement(Collection<ServiceUnit> serviceUnits){
		this.mServiceUnits = serviceUnits;
	}
	
	public boolean isEndpointActive(String consumingEndpointName)
			throws MBeanException {
		Endpoint ep = getEndpoint(consumingEndpointName);
		if(ep == null){
			String errMsg = mMessages.getString("JMSBC-E0328.EndPointDoesNotExist", consumingEndpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(ep.getState() == EndpointState.RUNNING )
			return true;
		else
			return false;
	}

	private Endpoint getEndpoint(String consumingEndpointName)
			throws MBeanException {
		String[] details = parseEndpointName(consumingEndpointName);
		QName service = new  QName(details[0], details[1]);
		String endpointName = details[2];
		int endpointType = details[3].equalsIgnoreCase("consumer")?EndpointType.INBOUND:EndpointType.OUTBOUND;
		for(Iterator<ServiceUnit> iter = mServiceUnits.iterator(); iter.hasNext();){
			ServiceUnit su = iter.next();
			Endpoint ep = su.getEndpoint(service.toString(), endpointName, endpointType);
			if(ep != null){
				return ep;
			}
		}
		return null;
	}

	public String[] listActiveEndpoints() {
		return getEndpoints(EndpointState.RUNNING);
	}

	private String[] getEndpoints(int state) {
		List<Endpoint> result = new ArrayList<Endpoint>(10);
		for(Iterator<ServiceUnit> iter = mServiceUnits.iterator(); iter.hasNext();){
			ServiceUnit su = iter.next();
			result.addAll(su.getEndpoints());
		}
		List<String> epNames = new ArrayList<String>(result.size());
		for(Iterator<Endpoint> iter = result.iterator(); iter.hasNext(); ){
			Endpoint ep = iter.next();
			if(ep.getState() == state)
				epNames.add(getEndpointName(ep));
		}
		return epNames.toArray(new String[0]);
	}

	private String getEndpointName(Endpoint ep) {
		return ep.getServiceName().getNamespaceURI()
				+ ","
				+ ep.getServiceName().getLocalPart()
				+ ","
				+ ep.getEndpointName()
				+ ","
				+ (ep.getEndpointType() == EndpointType.INBOUND ? "consumer"
						: "provider");
	}

	public String[] listInactiveEndpoints() {
		return getEndpoints(EndpointState.STOPPED);
	}

	public boolean resume(String consumingEndpointName) throws MBeanException {
		Endpoint ep = getConsumerEndpoint(consumingEndpointName);
		
		if(ep.getState() == EndpointState.RUNNING){
			String errMsg = mMessages.getString("JMSBC-E0333.EndpointAlreadyActive", consumingEndpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		try{
			ep.getServiceUnit().resume(ep);
		}catch(Exception t){
			String errMsg = mMessages.getString("JMSBC-E0330.CouldNotResumeEndpoint", consumingEndpointName);
			throw new MBeanException(t, errMsg); 
		}
		return true;
	}

	private Endpoint getConsumerEndpoint(String consumingEndpointName)
			throws MBeanException {
		Endpoint ep = getEndpoint(consumingEndpointName);
		if(ep == null){
			String errMsg = mMessages.getString("JMSBC-E0328.EndPointDoesNotExist", consumingEndpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(ep.getEndpointType() == EndpointType.OUTBOUND){
			String errMsg = mMessages.getString("JMSBC-E0332.CouldNotSuspendOrResumeProviderEndpoint", consumingEndpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(ep.getState() == EndpointState.SHUTDOWN){
			String errMsg = mMessages.getString("JMSBC-E0334.EndpointInShutdownState", consumingEndpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		return ep;
	}

	public boolean suspend(String consumingEndpointName) throws MBeanException {
		Endpoint ep = getConsumerEndpoint(consumingEndpointName);
		
		if(ep.getState() == EndpointState.STOPPED){
			String errMsg = mMessages.getString("JMSBC-E0335.EndpointAlreadySuspended", consumingEndpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		try{
			ep.getServiceUnit().suspend(ep);
		}catch(Exception t){
			String errMsg = mMessages.getString("JMSBC-E0329.CouldNotSuspendEndpoint", consumingEndpointName);
			throw new MBeanException(t, errMsg); 
		}
		return true;
	}
	
	private String[] parseEndpointName(String endpointName) throws MBeanException{
		List<String> result = new ArrayList<String>(5);
		for( StringTokenizer tokens = new StringTokenizer(endpointName, ","); tokens.hasMoreTokens(); ){
			result.add(tokens.nextToken().trim());
		}
		if(result.size() < 3 || result.size() > 4){
			String errMsg = mMessages.getString("JMSBC-E0327.EndpointNameInvalid", endpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(result.size() == 3)
			result.add("consumer");

		return result.toArray(new String[0]);
		
	}

}
