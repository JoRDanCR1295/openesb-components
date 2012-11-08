package org.glassfish.openesb.databasebc.mbeans;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.management.MBeanException;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;
import org.glassfish.openesb.databasebc.EndpointBean;
import org.glassfish.openesb.databasebc.JDBCBindingDeployer;

public class JDBCManagement implements JDBCManagementMBean {
	
    private static final Messages mMessages = Messages.getMessages(JDBCManagement.class);

	private JDBCBindingDeployer mServiceUnits;

	public JDBCManagement(JDBCBindingDeployer serviceUnits){
		this.mServiceUnits = serviceUnits;
	}
	
	public boolean isEndpointActive(String consumingEndpointName) throws MBeanException {
		EndpointBean ep = getEndpoint(consumingEndpointName);
		if(ep == null){
			String errMsg = mMessages.getString("JDBCBC-E01136.EndPointDoesNotExist", new Object[] { consumingEndpointName});
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(ep.getValue(EndpointBean.STATUS) == EndpointBean.STATUS_RUNNING )
			return true;
		else
			return false;
	}

	public String[] listActiveEndpoints() {
		return getEndpoints(EndpointBean.STATUS_RUNNING);
	}

	public String[] listInactiveEndpoints() {
		return getEndpoints(EndpointBean.STATUS_STOPPED);
	}

	public boolean resume(String consumingEndpointName) throws MBeanException {
		EndpointBean ep = getConsumerEndpoint(consumingEndpointName);
		
		if(ep.getValue(EndpointBean.STATUS) == EndpointBean.STATUS_RUNNING){
			String errMsg = mMessages.getString("JDBCBC-E01140.EndpointAlreadyActive",  new Object[] {consumingEndpointName });
			throw new MBeanException(new Exception(errMsg)); 
		}
		try {
			mServiceUnits.resume(ep);
		} catch(Exception t){
			String errMsg = mMessages.getString("JDBCBC-E01138.CouldNotResumeEndpoint", new Object[] { consumingEndpointName});
			throw new MBeanException(t, errMsg); 
		}
		return true;
	}

	public boolean suspend(String consumingEndpointName) throws MBeanException {
		EndpointBean ep = getConsumerEndpoint(consumingEndpointName);
		
		if(ep.getValue(EndpointBean.STATUS) == EndpointBean.STATUS_STOPPED){
			String errMsg = mMessages.getString("JDBCBC-E01142.EndpointAlreadySuspended",  new Object[] {consumingEndpointName});
			throw new MBeanException(new Exception(errMsg)); 
		}
		try {
			mServiceUnits.suspend(ep);
		} catch(Exception t){
			String errMsg = mMessages.getString("JDBCBC-E01137.CouldNotSuspendEndpoint",  new Object[] { consumingEndpointName});
			throw new MBeanException(t, errMsg); 
		}
		return true;
	}

	private EndpointBean getConsumerEndpoint(String consumingEndpointName) throws MBeanException {
		EndpointBean ep = getEndpoint(consumingEndpointName);
		if(ep == null){
			String errMsg = mMessages.getString("JDBCBC-E01136.EndPointDoesNotExist",  new Object[] { consumingEndpointName });
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(ep.getValue(EndpointBean.ENDPOINT_TYPE) == EndpointBean.ENDPOINT_TYPE_OUTBOUND){
			String errMsg = mMessages.getString("JDBCBC-E01139.CouldNotSuspendOrResumeProviderEndpoint",   new Object[] { consumingEndpointName});
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(ep.getValue(EndpointBean.STATUS) == EndpointBean.STATUS_SHUTDOWN){
			String errMsg = mMessages.getString("JDBCBC-E01141.EndpointInShutdownState",   new Object[] {consumingEndpointName });
			throw new MBeanException(new Exception(errMsg)); 
		}
		return ep;
	}

	private EndpointBean getEndpoint(String consumingEndpointName) throws MBeanException {
		String[] details = parseEndpointName(consumingEndpointName);
		QName service = new  QName(details[0], details[1]);
		String endpointName = details[2];
		String endpointType = details[3].equalsIgnoreCase("consumer")?EndpointBean.ENDPOINT_TYPE_INBOUND:EndpointBean.ENDPOINT_TYPE_OUTBOUND;
			
			EndpointBean[] epcol = mServiceUnits.getEndpoints().get(consumingEndpointName);
			EndpointBean ep = epcol[0];
			if(ep != null){
				return ep;
			}
		return null;
	}

	private String[] parseEndpointName(String endpointName) throws MBeanException{
		List<String> result = new ArrayList<String>(5);
		for( StringTokenizer tokens = new StringTokenizer(endpointName, ","); tokens.hasMoreTokens(); ){
			result.add(tokens.nextToken().trim());
		}
		if(result.size() < 3 || result.size() > 4){
			String errMsg = mMessages.getString("JDBCBC-E01135.EndpointNameInvalid", endpointName);
			throw new MBeanException(new Exception(errMsg)); 
		}
		if(result.size() == 3)
			result.add("consumer");

		return result.toArray(new String[0]);
		
	}

	private String[] getEndpoints(String state) {
		List<EndpointBean[]> result = new ArrayList<EndpointBean[]>(10);
		result.addAll(mServiceUnits.getEndpoints().values());
		List<String> epNames = new ArrayList<String>(result.size());
		for(Iterator<EndpointBean[]> iter = result.iterator(); iter.hasNext(); ){
			EndpointBean[] eparr = iter.next();
			EndpointBean ep = eparr[0];
			if(ep.getValue(EndpointBean.STATUS) == state)
				epNames.add(getEndpointName(ep));
		}
		return epNames.toArray(new String[0]);
	}

	private String getEndpointName(EndpointBean ep) {
		return ep.getUniqueName()+ (ep.getValue(EndpointBean.ENDPOINT_TYPE) == EndpointBean.ENDPOINT_TYPE_INBOUND ? "consumer"
						: "provider");
	}

}
