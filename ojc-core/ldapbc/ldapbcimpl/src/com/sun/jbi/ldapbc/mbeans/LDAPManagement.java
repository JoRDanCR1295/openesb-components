package com.sun.jbi.ldapbc.mbeans;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.management.MBeanException;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.EndpointImpl;
import com.sun.jbi.ldapbc.LDAPBindingDeployer;
//import com.sun.jbi.ldapbc.Endpoint.EndpointState;
//import com.sun.jbi.ldapbc.Endpoint.EndpointType;

public class LDAPManagement implements LDAPManagementMBean {

    private static final Messages mMessages = Messages.getMessages(LDAPManagement.class);
    private LDAPBindingDeployer mServiceUnits;

    public LDAPManagement(LDAPBindingDeployer serviceUnits) {
        this.mServiceUnits = serviceUnits;
    }

    public boolean isEndpointActive(String consumingEndpointName) throws MBeanException {
        EndpointImpl ep = getEndpoint(consumingEndpointName);
        if (ep == null) {
            String errMsg = mMessages.getString("LDAPBC-E01136.EndPointDoesNotExist", new Object[]{consumingEndpointName});
            throw new MBeanException(new Exception(errMsg));
        }
        if (ep.getState() == EndpointImpl.RUNNING) {
            return true;
        } else {
            return false;
        }
    }

    public String[] listActiveEndpoints() {
        return getEndpoints(EndpointImpl.RUNNING);
    }

    public String[] listInactiveEndpoints() {
        return getEndpoints(EndpointImpl.STOPPED);
    }

    public boolean resume(String consumingEndpointName) throws MBeanException {
        EndpointImpl ep = getConsumerEndpoint(consumingEndpointName);

        if (ep.getState() == EndpointImpl.RUNNING) {
            String errMsg = mMessages.getString("LDAPBC-E01140.EndpointAlreadyActive", new Object[]{consumingEndpointName});
            throw new MBeanException(new Exception(errMsg));
        }
        try {
            mServiceUnits.resume(ep);
        } catch (Exception t) {
            String errMsg = mMessages.getString("LDAPBC-E01138.CouldNotResumeEndpoint", new Object[]{consumingEndpointName});
            throw new MBeanException(t, errMsg);
        }
        return true;
    }

    public boolean suspend(String consumingEndpointName) throws MBeanException {
        EndpointImpl ep = getConsumerEndpoint(consumingEndpointName);

        if (ep.getState() == EndpointImpl.STOPPED) {
            String errMsg = mMessages.getString("LDAPBC-E01142.EndpointAlreadySuspended", new Object[]{consumingEndpointName});
            throw new MBeanException(new Exception(errMsg));
        }
        try {
            mServiceUnits.suspend(ep);
        } catch (Exception t) {
            String errMsg = mMessages.getString("LDAPBC-E01137.CouldNotSuspendEndpoint", new Object[]{consumingEndpointName});
            throw new MBeanException(t, errMsg);
        }
        return true;
    }

    private EndpointImpl getConsumerEndpoint(String consumingEndpointName) throws MBeanException {
        EndpointImpl ep = getEndpoint(consumingEndpointName);
        if (ep == null) {
            String errMsg = mMessages.getString("LDAPBC-E01136.EndPointDoesNotExist", new Object[]{consumingEndpointName});
            throw new MBeanException(new Exception(errMsg));
        }
        if (ep.getEndpointType() == EndpointImpl.OUTBOUND) {
            String errMsg = mMessages.getString("LDAPBC-E01139.CouldNotSuspendOrResumeProviderEndpoint", new Object[]{consumingEndpointName});
            throw new MBeanException(new Exception(errMsg));
        }
        if (ep.getState() == EndpointImpl.SHUTDOWN) {
            String errMsg = mMessages.getString("LDAPBC-E01141.EndpointInShutdownState", new Object[]{consumingEndpointName});
            throw new MBeanException(new Exception(errMsg));
        }
        return ep;
    }

    private EndpointImpl getEndpoint(String consumingEndpointName) throws MBeanException {
        String[] details = parseEndpointName(consumingEndpointName);
        QName service = new QName(details[0], details[1]);
        String endpointName = details[2];
        int endpointType = details[3].equalsIgnoreCase("consumer") ? EndpointImpl.INBOUND : EndpointImpl.OUTBOUND;

        EndpointImpl[] epcol = mServiceUnits.getEndpoint(service.toString(), endpointName, endpointType);
        EndpointImpl ep = epcol[0];
        if (ep != null) {
            return ep;
        }
        return null;
    }

    private String[] parseEndpointName(String endpointName) throws MBeanException {
        List<String> result = new ArrayList<String>(5);
        for (StringTokenizer tokens = new StringTokenizer(endpointName, ","); tokens.hasMoreTokens();) {
            result.add(tokens.nextToken().trim());
        }
        if (result.size() < 3 || result.size() > 4) {
            String errMsg = mMessages.getString("LDAPBC-E01135.EndpointNameInvalid", endpointName);
            throw new MBeanException(new Exception(errMsg));
        }
        if (result.size() == 3) {
            result.add("consumer");
        }
        return result.toArray(new String[0]);

    }

    private String[] getEndpoints(int state) {
        List<EndpointImpl[]> result = new ArrayList<EndpointImpl[]>(10);
        result.addAll(mServiceUnits.getEndpoints().values());
        List<String> epNames = new ArrayList<String>(result.size());
        for (Iterator<EndpointImpl[]> iter = result.iterator(); iter.hasNext();) {
            EndpointImpl[] eparr = iter.next();
            EndpointImpl ep = eparr[0];
            if (ep.getState() == state) {
                epNames.add(getEndpointName(ep));
            }
        }
        return epNames.toArray(new String[0]);
    }

    private String getEndpointName(EndpointImpl ep) {
        return ep.getServiceName().getNamespaceURI() + "," + 
                ep.getServiceName().getLocalPart() + "," + ep.getEndpointName() + 
                "," + (ep.getEndpointType() == EndpointImpl.INBOUND ? "consumer"
                : "provider");
    }
}
