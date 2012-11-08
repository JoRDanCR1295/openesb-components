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
 * @(#)HttpProxy.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.proxy;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.security.auth.HttpAuthenticator;
import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.FileInputStream;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Properties;

/*
 * It holds proxy information for HttpSoapBC.
 *
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */

public class HttpProxy {
    private static final Messages mMessages =
        Messages.getMessages(HttpProxy.class);
    private static final Logger mLog =
        Messages.getLogger(HttpProxy.class);

    private static final int DEFAULT_HTTP_PROXY_PORT = 80;
    private static final int DEFAULT_SOCKS_PROXY_PORT = 1080;
    
    private Proxy mProxy;
    private RuntimeConfigurationMBean mMBean;
    
    private boolean mUseJVMProxySettings = false;
    private Proxy.Type mProxyType = Proxy.Type.DIRECT; // SOCKS, HTTP, DIRECT
    private String mProxyHost;
    private int mProxyPort; // 80, 1080
    private String mNonProxyHosts; //"localhost|127.0.0.1";
    private String mProxyUserName;
    private char[] mProxyPassword;
    
    // internal config
    private static boolean useProxySelector = true; // use {true} now and make it configurable later on?
    //private boolean getMBeanPropertyDymamically = false;
    
    /** Creates a new instance of HttpProxy */
    public HttpProxy() {
        this(null);
    }
    
    /** Creates a new instance of HttpProxy */
    public HttpProxy(RuntimeConfigurationMBean mbean) {
    	this.mMBean = mbean;
        this.loadConfig(mbean);
        
        if (this.isUseJVMProxySettings() || Proxy.Type.DIRECT.equals(this.getProxyType())) {
            this.setProxy(Proxy.NO_PROXY);
        } else {
            SocketAddress sa = new InetSocketAddress(this.getProxyHost(), this.getProxyPort());
            this.setProxy(new Proxy(this.getProxyType(), sa));
        }
        
    }

    /** Loads config from properties file */
    private void loadConfig() {
        String dirName = System.getProperty("user.home", "/temp");
        String fileName = "HttpProxy.properties";
        Properties props = new Properties();
        try {
            FileInputStream fin = new FileInputStream(new File(dirName, fileName));
            props.load(fin);
        } catch (Exception ex) {
            this.setUseJVMProxySettings(false);
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, mMessages.getString("HTTPBC-W01130.HttpProxy_properties_load_failed", new Object[] {dirName, fileName}), ex);
            }
            return;
        }
        
        String prop = props.getProperty("useJVMProxySettings", "No");
        this.setUseJVMProxySettings("Yes".equalsIgnoreCase(prop) ? true : false);
        
        prop = props.getProperty("proxyType", "DIRECT");
        this.setProxyType("HTTP".equalsIgnoreCase(prop) ? Proxy.Type.HTTP : ("SOCKS".equalsIgnoreCase(prop) ? Proxy.Type.SOCKS : Proxy.Type.DIRECT));

        if (Proxy.Type.HTTP.equals(this.getProxyType())) {
            prop = props.getProperty("http.proxyHost", "");
            this.setProxyHost(prop);

            prop = props.getProperty("http.proxyPort", "80");
            this.setProxyPort(Integer.parseInt(prop));

            prop = props.getProperty("http.nonProxyHosts", "localhost|127.0.0.1");
            this.setNonProxyHosts(prop);
        } else if (Proxy.Type.SOCKS.equals(this.getProxyType())) {
            prop = props.getProperty("socksProxyHost", "");
            this.setProxyHost(prop);

            prop = props.getProperty("socksProxyPort", "1080");
            this.setProxyPort(Integer.parseInt(prop));
            
            prop = props.getProperty("socksNonProxyHosts", "localhost|127.0.0.1");
            this.setNonProxyHosts(prop);
        } else {
            // go for the default initial values
        }
        
        prop = props.getProperty("proxyUserName", "");
        this.setProxyUserName(prop);
        
        prop = props.getProperty("proxyPassword", "");
        this.setProxyPassword(prop.toCharArray());

    }

    /** Loads config from RuntimeConfigurationMBean */
    private void loadConfig(RuntimeConfigurationMBean mbean) {
        if (null == mbean) {
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, "HTTPBC-W01131.RuntimeConfigurationMBean_not_available");
            }
            this.loadConfig();
            return;
        }
        
        if (null != mbean.getUseJVMProxySettings()) {
            this.setUseJVMProxySettings(mbean.getUseJVMProxySettings().booleanValue());
        } else {
            this.setUseJVMProxySettings(true);
            try {
                mbean.setUseJVMProxySettings(new Boolean(this.isUseJVMProxySettings())); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "UseJVMProxySettings" + " = " + isUseJVMProxySettings());
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"UseJVMProxySettings", this.isUseJVMProxySettings()} ), ex);
                }
            }
        }

        if ("HTTP".equalsIgnoreCase(mbean.getProxyType())) {
            this.setProxyType(Proxy.Type.HTTP);
        } else if ("SOCKS".equalsIgnoreCase(mbean.getProxyType())) {
            this.setProxyType(Proxy.Type.SOCKS);
        } else if ("DIRECT".equalsIgnoreCase(mbean.getProxyType())) {
            this.setProxyType(Proxy.Type.DIRECT);
        } else {
            this.setProxyType(Proxy.Type.DIRECT);
            try {
                mbean.setProxyType(this.getProxyType().name()); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "ProxyType" + " = " + getProxyType().name());
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"ProxyType", this.getProxyType().name()} ), ex);
                }
            }
        }
        
        if (null != mbean.getProxyHost()) {
            this.setProxyHost(mbean.getProxyHost());
        } else {
            this.setProxyHost("");
            try {
                mbean.setProxyHost(this.getProxyHost()); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "ProxyHost" + " = " + getProxyHost());
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"ProxyHost", this.getProxyHost()} ), ex);
                }
            }
        }

        if (null != mbean.getProxyPort()) {
            this.setProxyPort(mbean.getProxyPort().intValue());
        } else {
            if (Proxy.Type.HTTP.equals(this.getProxyType())) {
                this.setProxyPort(DEFAULT_HTTP_PROXY_PORT);
            } else if (Proxy.Type.SOCKS.equals(this.getProxyType())) {
                this.setProxyPort(DEFAULT_SOCKS_PROXY_PORT);
            } else {
                this.setProxyPort(0);
            }
            try {
                mbean.setProxyPort(new Integer(this.getProxyPort())); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "ProxyPort" + " = " + getProxyPort());
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"ProxyPort", this.getProxyPort()} ), ex);
                }
            }
        }
        
        if (null != mbean.getNonProxyHosts()) {
            this.setNonProxyHosts(mbean.getNonProxyHosts());
        } else {
            this.setNonProxyHosts("");
            try {
                mbean.setNonProxyHosts(this.getNonProxyHosts()); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "NonProxyHosts" + " = " + getNonProxyHosts());
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"NonProxyHosts", this.getNonProxyHosts()} ), ex);
                }
            }
        }

        if (null != mbean.getProxyUserName()) {
            this.setProxyUserName(mbean.getProxyUserName());
        } else {
            this.setProxyUserName("");
            try {
                mbean.setProxyUserName(this.getProxyUserName()); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "ProxyUserName" + " = " + getProxyUserName());
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"ProxyUserName", this.getProxyUserName()} ), ex);
                }
            }
        }
        
        String proxyPassword = null;
        try {
            proxyPassword = mbean.retrieveProxyPassword(mMBean);
        } catch (Exception ex) {
            if (mLog.isLoggable(Level.SEVERE)) {
                mLog.log(Level.SEVERE, mMessages.getString("HttpProxy.loadConfig():_Failed_to_get_proxy_password_MBean_property"), ex);
            }
        }
        if (null != proxyPassword) {
            this.setProxyPassword(proxyPassword.toCharArray());
        } else {
            String defaultPass = "";
            this.setProxyPassword(defaultPass.toCharArray());
            try {
                mbean.setProxyPassword(defaultPass); // write back the default
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "Default MBean property " + "ProxyPassword" + " = " + defaultPass);
                }
            } catch (Exception ex) {
                if (mLog.isLoggable(Level.SEVERE)) {
                    mLog.log(Level.SEVERE, mMessages.getString("HTTPBC-E01101.Failed_default_mbean_property_write", new Object[] {"ProxyPassword", defaultPass} ), ex);
                }
            }
        }

    }
    
    public void register() {
        // always register
        //if (this.isUseJVMProxySettings()) {
        //    return;
        //}
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Registering HttpProxy...");
        }
        
        new HttpAuthenticator().registerHttpProxy(this);
        if (useProxySelector) {
            new HttpProxySelector(this).register();
        } else {
            new HttpProxyURLStreamHandlerFactory(this).register();
        }
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpProxy registered");
        }
    }

    public static void unregister() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Unregistering HttpProxy...");
        }
        
        HttpAuthenticator.unregisterHttpProxy();
        if (useProxySelector) {
            HttpProxySelector.unregister();
        } else {
            HttpProxyURLStreamHandlerFactory.unregister();
        }
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpProxy unregistered");
        }
    }

    public Proxy getProxy() {
        return this.mProxy;
    }
    
    public boolean isUseJVMProxySettings() {
        return this.mUseJVMProxySettings;
    }
    
    public Proxy.Type getProxyType() {
        return this.mProxyType;
    }
    
    public String getProxyHost() {
        return this.mProxyHost;
    }
    
    public int getProxyPort() {
        return this.mProxyPort;
    }
    
    public String getNonProxyHosts() {
        return this.mNonProxyHosts;
    }
    
    public String getProxyUserName() {
        return this.mProxyUserName;
    }
    
    public char[] getProxyPassword() {
        return this.mProxyPassword;
    }

    void setProxy(Proxy proxy) {
        this.mProxy = proxy;
    }

    void setUseJVMProxySettings(boolean useJVMProxySettings) {
        this.mUseJVMProxySettings = useJVMProxySettings;
    }

    void setProxyType(Proxy.Type proxyType) {
        this.mProxyType = proxyType;
    }

    void setProxyHost(String proxyHost) {
        this.mProxyHost = proxyHost;
    }

    void setProxyPort(int proxyPort) {
        this.mProxyPort = proxyPort;
    }

    void setNonProxyHosts(String nonProxyHosts) {
        this.mNonProxyHosts = nonProxyHosts;
    }

    void setProxyUserName(String proxyUserName) {
        this.mProxyUserName = proxyUserName;
    }

    void setProxyPassword(char[] proxyPassword) {
        this.mProxyPassword = proxyPassword;
    }

    public String toString() {
        return "UseJVMProxySettings=" + this.isUseJVMProxySettings() + "; " +
               "ProxyType=" + this.getProxyType() + "; " + 
               "ProxyHost=" + this.getProxyHost() + "; " + 
               "ProxyPort=" + this.getProxyPort() + "; " +
               "NonProxyHosts=" + this.getNonProxyHosts() + "; " +
               "ProxyUserName=" + this.getProxyUserName() + "; " +
               "ProxyPassword=******";
    }
    
}
