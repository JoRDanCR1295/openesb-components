/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JMSXARecoveryHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Properties;
import java.util.Map.Entry;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.Session;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.XAQueueConnection;
import javax.jms.XAQueueConnectionFactory;
import javax.jms.XAQueueSession;
import javax.jms.XATopicConnection;
import javax.jms.XATopicConnectionFactory;
import javax.jms.XATopicSession;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.naming.InitialContext;
import javax.resource.spi.ManagedConnection;
import javax.transaction.xa.XAResource;

import com.stc.jmsjca.core.XMCFQueueXA;
import com.stc.jmsjca.core.XMCFUnifiedXA;
import com.stc.jmsjca.unifiedjms.RAUnifiedResourceAdapter;

/**
 *
 * Helper class to deal with JMSJCA RA related wrapper classes
 */
public class JMSXARecoveryHelper {
    
    private ArrayList mcs = new ArrayList();
    private ArrayList mcfs = new ArrayList();
    private ArrayList xatcs = new ArrayList();
    private ArrayList xatss = new ArrayList();
    private ArrayList xaqcs = new ArrayList();
    private ArrayList xaqss = new ArrayList();
    private boolean allowCrashOnCommit = false;
        
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif =
                    (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                /*
                if (attrName.equals("AllowCrashOnCommit")) {  // todo: change this to use environment map
                    Boolean newVal = (Boolean) (attrNotif.getNewValue());
                    setAllowCrashOnCommit(newVal.booleanValue());
                } 
                 */
            }
        }
    };
    
    /** Creates a new instance of JMSSessionHelper */
    public JMSXARecoveryHelper() {
    }    
        
    public XAResource getXAResource (JMSConnectionInfoRecord rec) 
    throws Throwable {
        LoggableXAResource xar = null;
        
        if (rec instanceof JMSJndiConnectionInfoRecord) {
            // Get XAResource via JNDI lookup of xa connectionfactory
            xar = new LoggableXAResource(getXAResourceFromJndiCF((JMSJndiConnectionInfoRecord)rec));
        } else {
            // Initialized MCF once with connection url from first seen record
            //XMCFUnifiedXA mcf = new XMCFUnifiedXA();
            
            //cannot use unified mcf , as some jms provider only support jms 1.0
            XMCFQueueXA mcf = new XMCFQueueXA();
            RAUnifiedResourceAdapter ra = new RAUnifiedResourceAdapter();
            ra.setConnectionURL(rec.getConnectionURL());

            if (rec.getUsername() != null) {
                ra.setUserName(rec.getUsername());
            }
            if (rec.getPassword() != null) {
                ra.setPassword(rec.getPassword());
            }
            //set jmsjca options
            if(rec.getJmsjcaOptions() != null){
            	StringBuffer buf = new StringBuffer();
        		for(Entry<Object, Object> entry : rec.getJmsjcaOptions().entrySet()){
        			buf.append(entry.getKey());
        			buf.append("=");
        			buf.append(entry.getValue());
        			buf.append('\n');
        		}
                ra.setOptions(buf.toString());
            }

            mcf.setResourceAdapter(ra);
            mcfs.add(mcf);

            ConnectionFactory jmsConnFactory = 
                    (ConnectionFactory)mcf.createConnectionFactory();

            String overloadConnURL = getConnectionString(rec);
            // Use special JMSJCA createConnection method, overloading username
            Connection connection = null;
            Session session = null;
            if(jmsConnFactory instanceof TopicConnectionFactory) {
            	connection = ((TopicConnectionFactory)jmsConnFactory).createTopicConnection();
            	session = ((TopicConnection)connection).createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            } else if(jmsConnFactory instanceof QueueConnectionFactory){
            	connection = ((QueueConnectionFactory)jmsConnFactory).createQueueConnection();
            	session = ((QueueConnection)connection).createQueueSession(false, Session.AUTO_ACKNOWLEDGE);
            } else { //Generic Connection factory created by Look Up..
            	connection = jmsConnFactory.createConnection();
            	session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            }
            ManagedConnection mc = getManagedConnection(session);
            mcs.add(mc);
            session = null;
            connection = null;
            jmsConnFactory = null;
            xar = new LoggableXAResource(mc.getXAResource());
        }
        
        xar.logCalls(true);
        
        return xar;
    }
    
    private XAResource getXAResourceFromJndiCF (JMSJndiConnectionInfoRecord jmsJndi) throws Exception {
        XAResource xar = null;
        
        InitialContext ctx = null;
//        if (jmsJndi.getInitialContextFactory() == null) {
//            ctx = new InitialContext();
//        } else {
            Properties jndiProps = new Properties();
            if(jmsJndi.getInitialContextFactory() != null){
                jndiProps.setProperty("java.naming.factory.initial", jmsJndi.getInitialContextFactory());
            }
            if(jmsJndi.getProviderURL() != null){
                jndiProps.setProperty("java.naming.provider.url", jmsJndi.getProviderURL());            
            }
            if (jmsJndi.getSecurityPrincipal()!=null) {                
                jndiProps.setProperty("java.naming.security.principal", jmsJndi.getSecurityPrincipal());            
            }
            if (jmsJndi.getSecurityCredentials()!= null) {
                jndiProps.setProperty("java.naming.security.credentials", jmsJndi.getSecurityCredentials());                            
            }
            
            if (jmsJndi.getSecurityCredentials()!= null) {
                jndiProps.setProperty("java.naming.security.credentials", jmsJndi.getSecurityCredentials());                            
            }
            
            if (jmsJndi.getJndiEnv()!=null) {
                Iterator providerJndiEnvIter = jmsJndi.getJndiEnv().keySet().iterator();
                while (providerJndiEnvIter.hasNext()) {
                    String name = (String)providerJndiEnvIter.next();
                    String val = (String)jmsJndi.getJndiEnv().getProperty(name);
                    jndiProps.setProperty(name,val);
                }
            }
            if(jmsJndi.getJmsjcaOptions() != null){
                Iterator providerJndiEnvIter = jmsJndi.getJmsjcaOptions().keySet().iterator();
                while (providerJndiEnvIter.hasNext()) {
                    String name = (String)providerJndiEnvIter.next();
                    String val = (String)jmsJndi.getJmsjcaOptions().getProperty(name);
                    jndiProps.setProperty(name,val);
                }
            }
            ctx = new InitialContext(jndiProps);
//        }
        
        String cfName = null;
        if(jndiProps.getProperty("JMSJCA.TopicCF") != null){
        	cfName = jndiProps.getProperty("JMSJCA.TopicCF");
        }else if(jndiProps.getProperty("JMSJCA.QueueCF") != null){
        	cfName = jndiProps.getProperty("JMSJCA.QueueCF");
        }else{
        	cfName = jmsJndi.getConnectionFactoryName();
        }
        Object obj = ctx.lookup(cfName);
        
        if (obj instanceof XATopicConnectionFactory) {
            XATopicConnectionFactory xatcf = (XATopicConnectionFactory)obj;
            XATopicConnection xatc = null;
            if (jmsJndi.getUsername()!=null && jmsJndi.getPassword()!=null) {
                xatc = xatcf.createXATopicConnection(jmsJndi.getUsername(), jmsJndi.getPassword());
            } else {
                xatc = xatcf.createXATopicConnection();                
            }
            xatcs.add(xatc);
            XATopicSession xats = xatc.createXATopicSession();
            xatss.add(xats);
            xar = xats.getXAResource();
        } else if (obj instanceof XAQueueConnectionFactory) {
            XAQueueConnectionFactory xaqcf = (XAQueueConnectionFactory)obj;
            XAQueueConnection xaqc = null;
            if (jmsJndi.getUsername()!=null && jmsJndi.getPassword()!=null) {
                xaqc = xaqcf.createXAQueueConnection(jmsJndi.getUsername(), jmsJndi.getPassword());
            } else {
                xaqc = xaqcf.createXAQueueConnection();                
            }
            xaqcs.add(xaqc);
            XAQueueSession xaqs = xaqc.createXAQueueSession();
            xaqss.add(xaqs);
            xar = xaqs.getXAResource();            
        }
        
        return xar;
    }
        
    private void safeClose(InitialContext ctx) {
        if (ctx != null) {
            try {
                ctx.close();
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
    private ManagedConnection getManagedConnection(Session s) 
    throws Throwable {
        ManagedConnection mc = null;
        if (s != null) {
            com.stc.jmsjca.core.WSession ws = (com.stc.jmsjca.core.WSession)s;
            com.stc.jmsjca.core.JSession js = ws.getJSession();
            mc = js.getManagedConnection();
        }
        return mc;
    }            

    public static String getConnectionString(JMSConnectionInfoRecord rec) {
        String connectionURL = rec.getConnectionURL();
        String username = rec.getUsername();
        String password = rec.getPassword();
        String overloadedConnURL = connectionURL;
        if (username != null && password != null) {
            overloadedConnURL += "?user=" + username + "&password=" + password;
        }
        return overloadedConnURL;
    }    
    
    public void closeOpenedResources() {
        Iterator mcIt = mcs.iterator();
        while (mcIt.hasNext()) {
            ManagedConnection mc = (ManagedConnection)mcIt.next();
            try {
                mc.cleanup();
                mc.destroy();
            } catch (Throwable t) {
                continue;
            }
        }
        mcs.clear();
        mcfs.clear();

        Iterator xatsIter = xatss.iterator();
        while (xatsIter.hasNext()) {
            XATopicSession xats = (XATopicSession)xatsIter.next();
            try {
                xats.close();
            } catch (Throwable t) {
                continue;
            }
        }
        xatss.clear();
        
        Iterator xatcIter = xatcs.iterator();
        while (xatcIter.hasNext()) {
            XATopicConnection xatc = (XATopicConnection)xatcIter.next();
            try {
                xatc.close();
            } catch (Throwable t) {
                continue;
            }
        }
        xatcs.clear();
        
        Iterator xaqsIter = xaqss.iterator();
        while (xaqsIter.hasNext()) {
            XAQueueSession xaqs = (XAQueueSession)xaqsIter.next();
            try {
                xaqs.close();
            } catch (Throwable t) {
                continue;
            }
        }
        xaqss.clear();
        
        Iterator xaqcIter = xaqcs.iterator();
        while (xaqcIter.hasNext()) {
            XAQueueConnection xaqc = (XAQueueConnection)xaqcIter.next();
            try {
                xaqc.close();
            } catch (Throwable t) {
                continue;
            }
        }
        xaqcs.clear();
    }
    
    private void setAllowCrashOnCommit(boolean val) {
        allowCrashOnCommit = val;
    }
}
