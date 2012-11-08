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
 * @(#)BootstrapFilterChain.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.lifecycle;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.InstallationContext;
import javax.management.ObjectName;

import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Extensible filter chain for JBI bootstrap implementations.
 * <p>
 * Per the JBI spec, &quot;there is no guarantee that the same 
 * instance of its Bootstrap implementation will be used during 
 * both install and uninstall operations on the component. Data 
 * that need to be retained between installation-time and 
 * uninstallation-time must be persisted in such as fashion that 
 * a separate instance of the bootstrap class can find them, 
 * despite component or system shutdown.&quot;  Therefore, this
 * class uses a static map to hold instances of {@link BootstrapData}
 * in memory to account for component shutdowns. Future revisions
 * of this class will account for system shutdowns as well.
 * 
 * @author Kevan Simpson
 */
public abstract class BootstrapFilterChain implements Bootstrap {
	/**
	 * Simple persistence mechanism for Bootstrap instance data.
	 * Per the JBI spec, it is not guaranteed that the same instance of
	 * a Bootstrap will be used for both install and uninstall.
	 * This class provides a static in-memory map of Bootstrap data,
	 * keyed by the fully-qualified classname of this instance.
	 */
	private static Map<String, BootstrapData> mDataMap = new HashMap<String, BootstrapData>();
	
	/** Encapsulates instance data for this Bootstrap instance. */
	private BootstrapData mData = null;
	private Logger mLogger = null;
	
    public BootstrapFilterChain() {
        getData().setChain(new LinkedList<BootstrapFilter>());
    }

    public void addFilter(BootstrapFilter filter) {
        if (filter != null) {
        	getData().getChain().add(filter);
        	if (log().isLoggable(Level.FINER)) {
        		log().finer("CRL-2008: Adding filter \""+
        				    String.valueOf(filter) +
        				    "\" to component bootstrap for "+
        				    getData().getComponentContext().getComponentName());
        	}
        }
    }
    
    public boolean removeFilter(BootstrapFilter filter) {
    	if (filter != null) {
        	if (log().isLoggable(Level.FINER)) {
        		log().finer("CRL-2009: Removing filter \""+
        				    String.valueOf(filter) +
        				    "\" from component bootstrap for "+
        				    getData().getComponentContext().getComponentName());
        	}
    		
    		return getData().getChain().remove(filter);
    	}
    	else return false;
    }
    
    /** @see javax.jbi.component.Bootstrap#cleanUp() */
    public void cleanUp() throws JBIException {
		log().info(I18n.loc("CRL-5008: Cleaning up component {0}...", 
						  getData().getComponentContext().getComponentName()));
    	
        for (Iterator iter = getData().getChain().iterator(); iter.hasNext();) {
        	BootstrapFilter filter = null;
        	try {
        		filter = (BootstrapFilter) iter.next();
        		filter.cleanUp();
        	}
        	catch (Exception e) {
        		log().log(Level.WARNING,
    				  	  I18n.loc("CRL-6022: A bootstrap filter \"{0}\" failed during cleanUp: {1}",
    						     String.valueOf(filter), e.getMessage()),
    				      e);
        	}
        }
    }

    /** @see javax.jbi.component.Bootstrap#getExtensionMBeanName() */
    public ObjectName getExtensionMBeanName() {
        return getData().getExtensionMBeanName();
    }

    /** @see javax.jbi.component.Bootstrap#init(javax.jbi.component.InstallationContext) */
    public void init(InstallationContext installContext) throws JBIException {
    	mLogger = LogUtil.getLogger(installContext.getContext(), this.getClass().getName());
    	log().info(I18n.loc("CRL-5005: Initializing component bootstrap for {0}", 
    						installContext.getComponentName()));
    	
    	getData().setInstallationContext(installContext);
    	getData().setComponentContext(installContext.getContext());
        for (Iterator iter = getData().getChain().iterator(); iter.hasNext();) {
        	BootstrapFilter filter = null;
        	try {
        		filter = (BootstrapFilter) iter.next();
        		filter.init(installContext);
        	}
        	catch (Exception e) {
        		log().log(Level.WARNING,
      				  	  I18n.loc("CRL-6023: A bootstrap filter \"{0}\" failed during init: {1}",
      						     String.valueOf(filter), e.getMessage()),
      				      e);
        	}
        }
    }

    /** @see javax.jbi.component.Bootstrap#onInstall() */
    public void onInstall() throws JBIException {
    	log().info(I18n.loc("CRL-5006: Installing component {0}...", 
    					  getData().getComponentContext().getComponentName()));
    	
        for (Iterator iter = getData().getChain().iterator(); iter.hasNext();) {
        	BootstrapFilter filter = null;
        	try {
        		filter = (BootstrapFilter) iter.next();
        		filter.onInstall();
        	}
        	catch (Exception e) {
        		log().log(Level.WARNING,
        				  I18n.loc("CRL-6024: A bootstrap filter \"{0}\" failed during onInstall: {1}",
        						 String.valueOf(filter), e.getMessage()),
        				  e);
        	}
        }
    }

    /** @see javax.jbi.component.Bootstrap#onUninstall() */
    public void onUninstall() throws JBIException {
    	log().info(I18n.loc("CRL-5007: Uninstalling component {0}...", 
				   		  getData().getComponentContext().getComponentName()));

        for (Iterator iter = getData().getChain().iterator(); iter.hasNext();) {
        	BootstrapFilter filter = null;
        	try {
        		filter = (BootstrapFilter) iter.next();
        		filter.onUninstall();
        	}
        	catch (Exception e) {
        		log().log(Level.WARNING,
        				  I18n.loc("CRL-6025: A bootstrap filter \"{0}\" failed during onUninstall: {1}",
        						 String.valueOf(filter), e.getMessage()),
        				  e);
        	}
        }
    }
    
    /** Fetches the component context of this bootstrap instance. */
    public ComponentContext getComponentContext() {
        return getData().getComponentContext();
    }
    
    /** Fetches the installation context of this bootstrap instance. */
    public InstallationContext getInstallationContext() {
        return getData().getInstallationContext();
    }

    /** Sets the extension object name. */
    public void setExtensionMBeanName(ObjectName extensionMBeanName) {
    	getData().setExtensionMBeanName(extensionMBeanName);
    }

    /** @see java.lang.Object#toString() */
    public String toString() {
        // TODO may be too expensive
        StringBuffer buff = new StringBuffer(getClass().getName());
        buff.append("[");
        Iterator iter = getData().getChain().iterator(); 
        if (iter.hasNext()) {
            buff.append(String.valueOf(iter.next()));
            while (iter.hasNext()) {
                buff.append(",").append(String.valueOf(iter.next()));
            }
        }
        buff.append("]");
        return buff.toString();
    }
    
    // protected accessors/mutators

    protected BootstrapData getData() {
    	// TODO must this method be synchronized
    	// does this instance have its data?
    	if (mData == null) {
    		mData = mDataMap.get(this.getClass().getName());
    		if (mData == null) {	// new instance, store data
    			mData = createData();
    		}
    	}
    	// is instance data persisted?
    	if (mDataMap.get(this.getClass().getName()) == null) {
    		mDataMap.put(this.getClass().getName(), mData);
    	}
    	
    	return mData;
    }
     
    /**
     * Creates an instance of {@link BootstrapData}. Subclasses requiring more 
     * advanced data objects can override this method.
     * 
     * @return a newly created <code>BootstrapData</code> instance.
     */
    protected BootstrapData createData() {
    	return new BootstrapData();
    }
    
    /** Fetches a non-null {@link Logger} for this bootstrap filter. */ 
    protected Logger log() {
    	return ((mLogger == null) ? Logger.getLogger(this.getClass().getName()) 
    							  : mLogger);
    }
}
