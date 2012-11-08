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
 * @(#)DeploymentLookup.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import org.xml.sax.InputSource;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.descriptor.model.AssemblyUnit;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.util.Util;

/**
 * Utility to provide lookup functionality against a component's deployed
 * service assemblies.
 * 
 * @author Kevan Simpson
 */
public class DeploymentLookup {
    private static final String EXT_MTHD_NM = "getServiceAssemblyName";
    
    private enum Mthd {
        getDeployedServiceAssembliesForComponent,   // standard methods
        getServiceAssemblyDescriptor,
        getServiceAssemblyName // the extension method
    }
    
	private ComponentContext mCtx;
	private Logger mLogger;
    private AssemblyCache mAssemblyByUnitCache;	// keyed by SU name
    private AssemblyCache mNamedAssemblyCache;	// keyed by SA name
    private ObjectName mObjectName;
    
	public DeploymentLookup(ComponentContext ctx) throws DeploymentException {
		mCtx = ctx;
        mLogger = Util.getLogger(mCtx, DeploymentLookup.class.getName());
        mObjectName = createMBean();
        mAssemblyByUnitCache = new AssemblyCache();
        mNamedAssemblyCache = new AssemblyCache();
	}

	
    /**
     * Fetches the {@link ServiceAssembly} for the specified service unit, by name.
     * 
     * @param serviceUnitName The name of a service unit.
     * @return The service assembly of which the service unit is a part.
     * @throws DeploymentException If an error occurs accessing or parsing descriptors.
     * @see #getQoSAssembly(String)
     */
    public ServiceAssembly getServiceAssembly(String serviceUnitName) 
            throws DeploymentException {
    	return getQoSAssembly(serviceUnitName);
    }
    
    /**
     * Fetches the {@link QoSAssembly} (including {@link ServiceQuality}
     * configurations) for the specified service unit, by name.
     * 
     * @param serviceUnitName The name of a service unit.
     * @return The service assembly of which the service unit is a part.
     * @throws DeploymentException If an error occurs accessing or parsing descriptors.
     */
    public QoSAssembly getQoSAssembly(String serviceUnitName) throws DeploymentException {
        try {
        	return loadAssembly(null, serviceUnitName);
        }
        catch (DeploymentException de) {
            // already logged, rethrow
            throw de;
        }
        catch (Exception e) {
            throw deployError(e, "QOS-6052: Failed to acquire Service Assembly for Service Unit {0}: {1}",
                              serviceUnitName, e.getMessage());
        }
    }

    /**
     * Fetches all consumer endpoints connected to the specified provider endpoint.
     * 
     * @param provider The specified provider endpoint.
     * @return An array of consumers of the specified endpoint.
     * @throws DeploymentException if an error occurs accessing or parsing descriptors.
     * @throws IllegalArgumentException if the specified provider is <code>null</code>.
     */
    public EndpointInfo[] getConsumersByProvider(String serviceUnitName, EndpointInfo provider) 
            throws DeploymentException {
        try {
            if (provider == null) {
                throw new IllegalArgumentException(I18n.loc(
                        "QOS-6018: Provider endpoint cannot be NULL!"));
            }
            
        	// check SA cache
        	QoSAssembly sa = loadAssembly(null, serviceUnitName);
            
            return (sa == null) 
            		? new EndpointInfo[0] : sa.getConsumersByProvider(provider);
        }
        catch (DeploymentException de) {
            // already logged, rethrow
            throw de;
        }
        catch (Exception e) {
            throw deployError(e, "QOS-6051: Failed to acquire Consumers for {0} Provider {1}: {2}",
                              getContext().getComponentName(), provider.getEndpointName(), e.getMessage());
        }
    }
    
    /**
     * Fetches all consumer endpoints connected to the specified provider endpoint.
     * 
     * @param provider The specified provider endpoint.
     * @return An array of consumers of the specified endpoint.
     * @throws DeploymentException if an error occurs accessing or parsing descriptors.
     * @throws IllegalArgumentException if the specified provider is <code>null</code>.
     * @deprecated Please use {@link #getConsumersByProvider(String, EndpointInfo)}.
     */
    public EndpointInfo[] getConsumersByProvider(EndpointInfo provider) 
            throws DeploymentException {
        try {
            if (provider == null) {
                throw new IllegalArgumentException(I18n.loc(
                        "QOS-6018: Provider endpoint cannot be NULL!"));
            }

            List<EndpointInfo> consumers = new ArrayList<EndpointInfo>();
            String[] saNames = (String[]) invoke(
                    Mthd.getDeployedServiceAssembliesForComponent, 
                    mCtx.getComponentName());
            if (saNames != null) {
                for (String name : saNames) {
    	        	// check SA cache
                	QoSAssembly sa = loadAssembly(name, null);
    	        	if (sa == null) {
    	        		// TODO log
    	        		// cache tries to load... if cache can't get, give up...
    	        		continue;
    	        	}
    	        	
    	        	EndpointInfo[] endpts = sa.getConsumersByProvider(provider);
    	        	if (endpts != null) {
    		            for (EndpointInfo info : endpts) {
    		            	consumers.add(info);
    		            }
    	        	}
                }
            }
            
            EndpointInfo[] endpts = new EndpointInfo[consumers.size()];
            consumers.toArray(endpts);
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.finest("QOS-1002: Provider("+ String.valueOf(provider) 
                               +") has Consumers: \n\t"+ Util.toString(endpts, "\n\t"));
            }
            return endpts;
        }
        catch (DeploymentException de) {
            // already logged, rethrow
            throw de;
        }
        catch (Exception e) {
            throw deployError(e, "QOS-6051: Failed to acquire Consumers for {0} Provider {1}: {2}",
                              getContext().getComponentName(), provider.getEndpointName(), e.getMessage());
        }
    }
    
    /**
     * Fetches a list of {@link ServiceQuality} configurations for the specified
     * service unit, by installation root path.
     * 
     * @param serviceUnitRootPath The installation root path of a service unit.
     * @return a map of the service unit's consumers to their configured service qualities.
     * @throws DeploymentException if an error occurs accessing or parsing descriptors.
     * @deprecated Please use {@link #getQoSAssembly(String)} as this 
     *             method looks across service assemblies, which is very expensive.
     */
	public Map<EndpointInfo, List<ServiceQuality>> // 
			lookupServiceQualities(String serviceUnitRootPath) throws DeploymentException {
		try {
			/*
			 * According to Mark White and Nikita Sawant, it is guaranteed that
			 * the SU name can be derived from the SU root path.
			 * I'm taking it one step further and taking the SA name directly,
			 * to avoid the extra lookup to acquire SA name from SU name.
			 * ...
			 * Assuming SU install path of: <sa-name>/<su-name>/<component-name>
			 * 
			 * UPDATE: Which does NOT apply in v3/Fuji! Parse the SU descriptor.
			 * 
			 * This call is very expensive!
			 */
		    ServiceUnit su = ServicesDescriptor
		            .parse("DeploymentLookupSU", serviceUnitRootPath);
            List<EndpointInfo> list = new ArrayList<EndpointInfo>();
            list.addAll(Arrays.asList(su.getServices().getConsumes()));
            String[] saNames = (String[]) invoke(
                    Mthd.getDeployedServiceAssembliesForComponent, 
                    mCtx.getComponentName());
            Map<EndpointInfo, List<ServiceQuality>> endpts = 
                new HashMap<EndpointInfo, List<ServiceQuality>>();

            if (saNames != null) {
    			for (String assmName : saNames) {
    			    if (list.isEmpty()) {
    			        break;   // all qos found
    			    }
    			    
    			    QoSAssembly sa = loadAssembly(assmName, null);
    			    Map<EndpointInfo, List<ServiceQuality>> map =
    			            sa.getServiceQualities();
    			    for (Iterator<EndpointInfo> iter = list.iterator(); iter.hasNext();) {
    			        EndpointInfo info = iter.next();
    			        List<ServiceQuality> qos = map.get(info);
    			        if (qos != null && !qos.isEmpty()) {
    			            // once found, remove from list to avoid add'l checks
    			            endpts.put(info, qos);
    			            iter.remove();
    			        }
    			    }
    			}
            }
            
			return endpts;
        }
        catch (DeploymentException de) {
            // already logged, rethrow
            throw de;
        }
        catch (Exception e) {
            throw deployError(e, "QOS-6050: Failed to lookup {0} Service Qualities: {1}",
                              getContext().getComponentName(), e.getMessage());
        }
    }
    
    /**
     * Fetches a list of {@link ServiceQuality} configurations for the specified
     * array of consumer endpoints.
     * 
     * @param consumers The endpoints for which service qualities are retrieved.
     * @return a map of the consumers to their configured service qualities.
     * @throws DeploymentException if an error occurs accessing or parsing descriptors.
     * @deprecated Please use {@link #getQoSAssembly(String)} as this 
     * 			   method looks across service assemblies, which is very expensive.
     */
    public Map<EndpointInfo, List<ServiceQuality>>
            lookupServiceQualities(EndpointInfo... consumers) throws DeploymentException {
        try {
            // build list, so we can remove entries as we build map
            List<EndpointInfo> list = new ArrayList<EndpointInfo>();
            for (EndpointInfo ei : consumers) {
                list.add(ei);
            }

            Map<EndpointInfo, List<ServiceQuality>> qos, map =
                    new HashMap<EndpointInfo, List<ServiceQuality>>();            
			String[] saNames = (String[]) invoke(
                    Mthd.getDeployedServiceAssembliesForComponent, 
                    mCtx.getComponentName()); 
            if (saNames != null) {
    			/*
    			 * 	LOGIC FROM KEITH
    				  for each <consumes> element in SU jbi.xml
    				     find qos:connection in SA jbi.xml with consuming endpoint name =
    				     <consumes> endpoint name in SU jbi.xml;
    				     add QoS to returned map with key = endpoint name;
    				     
    				I think it's more expensive to access SA descriptors, so I'm going with...
    			 */
    			for (String sa : saNames) {
    	        	// check SA cache
    				QoSAssembly assm = loadAssembly(sa, null);
    	        	if (assm == null) {
    	        		// TODO log
    	        		// loadAssembly checks cache and tries to load... null=done 
    	        		continue;
    	        	}
    	        	
    	        	qos = assm.getServiceQualities();	// map of consumers to QoS list
    	        	Connection[] cons = assm.getQoSConnections();
    				int index = -1;
    				for (Connection con : cons) {
    					if ((index = list.indexOf(con.getConsumer())) >= 0) {
    						map.put(list.remove(index), qos.get(con.getConsumer()));
    					}
    				}
    			}
            }
            
			return map;
		}
		catch (DeploymentException de) {
			// already logged, rethrow
			throw de;
		}
		catch (Exception e) {
			throw deployError(e, "QOS-6050: Failed to lookup {0} Service Qualities: {1}",
			                  getContext().getComponentName(), e.getMessage());
		}
	}
	
    public void notifyUndeployed(String serviceUnitName) {
    	try {
    		QoSAssembly sa = mAssemblyByUnitCache.uncacheAssembly(serviceUnitName);
    		if (sa == null) {
    		    // look up by SA name
    		    String assmName = getAssemblyName(serviceUnitName);
    		    if (!Util.isEmpty(assmName)) {
    		        mNamedAssemblyCache.uncacheAssembly(assmName);
    		    }
    		}
    		else {
    		    mNamedAssemblyCache.uncacheAssembly(sa);
    		}
    	}
    	catch (Exception e) {
        	log().log(Level.FINE, I18n.format(
        			"QOS-3004: An error occurred trying to clear assembly cache: {0}", 
        			e.getMessage()), e);
    	}
    }
    
    protected Object invoke(Mthd mthd, Object... args) throws DeploymentException {
        if (mObjectName != null) {
            try {
                int len = (args != null) ? args.length : 0;
                String[] signature = new String[len];
                for (int i = 0; i < len; i++) {
                    signature[i] = args[i].getClass().getName();
                }

                return mCtx.getMBeanServer().invoke(
                        mObjectName, mthd.name(), args, signature);
            }
            catch (Exception e) {
                switch (mthd) {
                    case getServiceAssemblyName: {
                        // log failure, but continue attempting to lookup the hard way
                        log().log(Level.INFO, I18n.loc(
                                "QOS-5001: Unable to dynamically invoke {0} on mbean {1}", 
                                mthd.name(), mObjectName.toString()), e);
                        return null;
                    }
                    default: {
                        String msg = I18n.loc(
                                "QOS-6058: Failed to dynamically invoke {0} on mbean {1}", 
                                mthd.name(), mObjectName.toString());
                        log().log(Level.WARNING, msg, e);
                        throw new DeploymentException(msg, e);
                    }
                }
            }
        }

        return null;
    }
    
    protected QoSAssembly loadAssembly(String saName, String suName) 
    		throws Exception {
    	// check SA caches, keyed by SU name first, then by SA name
    	QoSAssembly sa = mAssemblyByUnitCache.getAssembly(suName);
    	if (sa != null) {
    		return sa;
    	}
    	else if (saName != null) {
    		sa = mNamedAssemblyCache.getAssembly(saName);
    		if (sa != null) {
    			return sa;
    		}
    	}
    	
        // prefer SA name for lookup, but if it's null, use SU name
        String assmName = (saName != null) ? saName 
                : getAssemblyName(suName);
        // maybe we didn't have SA name before, check cache one more time
        sa = mNamedAssemblyCache.getAssembly(assmName);
        // still not found, look up SA
        if (sa == null) {
	        String saXml = (String) invoke(
	                Mthd.getServiceAssemblyDescriptor, assmName);
	        if (!Util.isEmpty(saXml)) {
    	        sa = QosAssemblyDescriptor.parse(
    	        		new InputSource(new StringReader(saXml)));
    	        assmName = sa.getIdentification().getName();
    	        mNamedAssemblyCache.cacheAssembly(assmName, sa);
	        }
        }
        
        // cache by SU name
        if (sa != null && suName != null) {
            mAssemblyByUnitCache.cacheAssembly(suName, sa);
        }
        
        return sa;
    }
    
    /*
     * This method tries to resolve the SA name for an SU while the
     * dependency on jbi-ext.jar is fixed by attempting to dynamically
     * invoke the extension mbean method.
     */
    private String getAssemblyName(String suName) throws Exception {
        // extension mbean available, lookup SA name directly
        if (mObjectName != null) {
            Object value = null; 
            try {
                Object[] args = { suName, mCtx.getComponentName() };
                String str = String.class.getName(), signature[] = { str, str };
                value = mCtx.getMBeanServer().invoke(
                        mObjectName, EXT_MTHD_NM, args, signature);
                if (value != null) {
                    return String.valueOf(value);
                }
                // else fall through and lookup the hard way
            }
            catch (Exception e) {
                // log failure, but continue attempting to lookup the hard way
                log().log(Level.FINE, I18n.format(
                        "QOS-3005: Unable to invoke {0} dynamically for service unit - {1}: {2}", 
                        mObjectName.toString(), suName, e.getMessage()));
            }
        }

        // if extension mbean fails or is absent, look the hard way
        String compName = mCtx.getComponentName();
        String[] names = (String[]) invoke(
                Mthd.getDeployedServiceAssembliesForComponent, compName); 
        if (names != null) {
            // iterate through SA names and load/parse descriptors
            for (int i = 0, I = names.length; i < I; i++) {
                String xml = (String) invoke(
                        Mthd.getServiceAssemblyDescriptor, names[i]);
                if (!Util.isEmpty(xml)) {
                    QoSAssembly sa = QosAssemblyDescriptor.parse(
                            new InputSource(new StringReader(xml)));
                    // cache in saMap
                    mNamedAssemblyCache.cacheAssembly(
                            sa.getIdentification().getName(), sa);
                    // iterate through SA's units for match, caching along the way...
                    AssemblyUnit[] units = sa.getServiceUnits();
                    for (int j = 0, J = units.length; j < J; j++) {
                        if (units[j].getTarget().getComponentName().equals(compName)) {
                            // cache in suMap
                            String id = units[j].getId().getName();
                            mAssemblyByUnitCache.cacheAssembly(id, sa);
                            if (id.equals(suName)) {
                                return sa.getIdentification().getName();
                            }
                        }
                    }
                }
            }
        }
        
        return null;
    }
    
    protected ObjectName createMBean() throws DeploymentException {
        // only works for OpenESB
        // TODO add logic to try other implementations
        try {
            // to support unit tests, do not create MBean if ComponentCtx is null
            if (mCtx == null) {
                return null;
            }
            
            ObjectName objName = new ObjectName(   // a query for DS mbean
                    "*:ControlType=DeploymentService,ServiceName=DeploymentService,*");
            MBeanServer mbs = mCtx.getMBeanServer();
            Set mbeans = mbs.queryNames(objName, null);
            if (mbeans == null || mbeans.isEmpty()) {
                return null;   // calling method is expected to log this...
            }
            
            // Should be the first (and only) entry in the list for Open ESB
            return (ObjectName) mbeans.toArray()[0];
        }
        catch (MalformedObjectNameException mone) {
            throw deployError(mone, 
                    "QOS-6049: Failed to create DeploymentServiceMBean for {0}: {1}",
                    getContext().getComponentName(), mone.getMessage());
        }
    }

	protected ComponentContext getContext() {
	    return mCtx;
	}
	protected Logger log() {
	    return mLogger;
	}
	
	private DeploymentException deployError(Exception e, String msg, Object... params) {
	    String err = I18n.loc(msg, params);
	    if (e == null) {
	        log().warning(err);
	        return new DeploymentException(err);
	    }
	    else {
	        log().log(Level.WARNING, err, e);
	        return new DeploymentException(err, e);
	    }
	}
	
	public static class AssemblyCache {
	    private Map<String, QoSAssembly> mAssemblyMap;
	    
	    public AssemblyCache() {
	        mAssemblyMap = new HashMap<String, QoSAssembly>();
	    }

	    public void cacheAssembly(String key, QoSAssembly sa) {
	    	if (sa != null) {
	    		synchronized (this) {
	    			mAssemblyMap.put(key, sa);
				}
	    	}
	    }

	    public QoSAssembly getAssembly(String key) {
	    	if (!Util.isEmpty(key)) {
		    	synchronized (this) {
		    		return mAssemblyMap.get(key);
				}
	    	}
	    	
	    	return null;
	    }

        public QoSAssembly uncacheAssembly(QoSAssembly sa) {
            if (sa != null) {
                synchronized (this) {
                    if (mAssemblyMap.containsValue(sa)) {
                        for (String key : mAssemblyMap.keySet()) {
                            QoSAssembly other = mAssemblyMap.get(key);
                            if (other != null && sa.getIdentification().getName()
                                    .equals(other.getIdentification().getName())) {
                                return mAssemblyMap.remove(key);
                            }
                        }
                    }
                }
            }
            
            return null;
        }

	    public QoSAssembly uncacheAssembly(String key) {
    		synchronized (this) {
    			return mAssemblyMap.remove(key);
			}
	    }
	}
}
