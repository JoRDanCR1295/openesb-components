/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 * @(#)CamelSEServiceUnit.java 
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.openesb.components.camelse;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import org.apache.camel.CamelContext;
import org.openesb.components.camelse.camel.JBIBridgeComponent;
import org.openesb.components.camelse.camel.JBIBridgeEndpoint;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.deployment.ConsumerEndpoint;
import org.openesb.components.camelse.common.deployment.SUDescriptor.Consumes;
import org.openesb.components.camelse.common.deployment.ServiceUnit;
import org.openesb.components.camelse.common.deployment.ProviderEndpoint;
import org.openesb.components.camelse.common.deployment.SUDescriptor.Provides;
import java.io.File;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;

/**
 * This class extends the ServiceUnit to implement the component specific service unit processing.
 * It creates the component specific ProviderEndpoint implementation to configure the service
 * provider endpoint on this component deployed in the service unit. It also processes the deployment
 * artifacts that are specific to the service provided by the component.
 * 
 * @author chikkala
 */
public class CamelSEServiceUnit extends ServiceUnit {

    private CamelSERuntime mRuntime;
    private Object mCamelMainObj;
    private Class mCamelMainClazz;

    /** Creates a new instance of CamelSEServiceUnit */
    public CamelSEServiceUnit(String suName, String suRootPath, CamelSERuntime runtime) {
        super(suName, suRootPath);
        this.mRuntime = runtime;
    }

    @Override
    protected ProviderEndpoint createProviderEndpoint(Provides provides, Definition wsdlDef) {
        return new CamelSEProviderEndpoint(provides, wsdlDef, this);
    }

    @Override
    protected ConsumerEndpoint createConsumerEndpoint(Consumes consumes, Definition wsdlDef) {
        return new CamelSEConsumerEndpoint(consumes, wsdlDef, this);
    }

    /**
     * load and validates the component specific deployment artifacts. It loads the xsltmap
     * properties into XSLTFileLocator which will be used to find the xslt file corresponding
     * to a service operation.
     */
    @Override
    protected void loadOtherArtifacts() throws DeploymentException {
        super.loadOtherArtifacts();
    // load any component specific service unit artifacts
    }
    private List<String> getPaths(String baseDir, String pattern) {
        List<String> list = new ArrayList<String>();
        File baseFile = new File(baseDir);
        if (!baseFile.exists()) {
            return list;
        }
        //TODO: check the pattern for wildcards also.
        String[] libs = pattern.split(",");
        for ( String lib : libs) {
            File path = new File(baseFile, lib);
            if ( path.exists()) {
                list.add(path.getAbsolutePath());
            } else {
                RuntimeHelper.logDebug("Path for SU classpath does not exist " + path.getPath());
            }
        }
        return list;
    }
    /**
     * Additional Camel libraries configured using component configuration.
     * compute the list of libraries from includes and exlcudes of the camel
     * libraries configured.
     * @return
     */
    private List<URL> getAdditionalCamelClassPath() {
        List<URL> cp = new ArrayList<URL>();
        CamelSEConfigMBean configMBean = null;
        try {
            //TODO: we can get the configuration via mbean lookup and can avoid runtime dependency.
            CamelSEComponentLifeCycle lc = (CamelSEComponentLifeCycle) this.mRuntime.getLifeCycle();
            configMBean = lc.getConfigMBeanImpl();
        } catch (Exception ex) {
            RuntimeHelper.logDebug(ex);
        }
        if ( configMBean == null ) {
            return cp;
        }
        String camelHome = configMBean.getCamelHome();
        String includes = configMBean.getIncludeCamelLibs();
        String excludes = configMBean.getExcludeCamelLibs();
        
        if ( camelHome == null || camelHome.trim().length() == 0 ) {
            return cp;
        }
        if ( includes == null || includes.trim().length() == 0 ) {
            return cp;
        }
        if ( includes == null || includes.trim().length() == 0 ) {
            return cp;
        }        
        List<String> includePaths = getPaths(camelHome, includes);
        List<String> excludePaths = getPaths(camelHome, excludes);
        includePaths.removeAll(excludePaths);
        for ( String path : includePaths ) {
            try {
                URL pathURL = (new File(path)).toURL();
                cp.add(pathURL);
            } catch (MalformedURLException ex) {
                Logger.getLogger(CamelSEServiceUnit.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return cp;
    }

    /**
     * classpath url for the camel application code. currently the su root.
     * //TODO: this may include application specific libraries 
     * @return
     */
    private List<URL> getCamelAppClassPath() {
        List<URL> cp = new ArrayList<URL>();
        try {
            String path = this.getSURootPath();
            URL pathURL = (new File(path)).toURL();
            cp.add(pathURL);
        } catch (MalformedURLException ex) {
            Logger.getLogger(CamelSEServiceUnit.class.getName()).log(Level.SEVERE, null, ex);
        }
        return cp;
    }

    /**
     * return the classloader for the camel app including additional camel libraries
     * configured using config mbean.
     * @return
     */
    private ClassLoader createCamelSUClassLoader() {
        List<URL> addlCP = getAdditionalCamelClassPath();
        List<URL> appCP = getCamelAppClassPath();
        List<URL> camelCP = new ArrayList<URL>();
        //TODO: add the file in the same order they were in the two lists.
        camelCP.addAll(addlCP);
        camelCP.addAll(appCP);
        
        URL[] cpURLs = camelCP.toArray(new URL[camelCP.size()]);

        URLClassLoader loader = new URLClassLoader(cpURLs, this.getClass().getClassLoader());

        RuntimeHelper.logDebug("### CamelSU Classpath ###");
        StringBuffer buff = new StringBuffer();
        for (URL cpURL : cpURLs) {
            buff.append(cpURL).append(",");
        }
        RuntimeHelper.logDebug(buff);

        return loader;
    }

    private void startCamel(ClassLoader loader) {
        RuntimeHelper.getLogger().fine("Loading camel in su classloader " + loader);
        try {

            loader.loadClass("org.slf4j.LoggerFactory");

            mCamelMainClazz = loader.loadClass("org.apache.camel.spring.Main");

            mCamelMainObj = mCamelMainClazz.newInstance();
            Method startMethod = mCamelMainClazz.getMethod("start", null);
            startMethod.invoke(mCamelMainObj, null);
            //this.mCamelMain = (Main) obj;
            //this.mCamelMain.start();
        } catch (Exception ex) {
            RuntimeHelper.logError(ex);
        }
    }

    private void startCamelInThreadContext() {
        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
        try {
            ClassLoader newLoader = createCamelSUClassLoader();
            Thread.currentThread().setContextClassLoader(newLoader);
            startCamel(newLoader);
        } finally {
            Thread.currentThread().setContextClassLoader(oldClassLoader);
        }
    }

    @Override
    public void doStart() throws DeploymentException {
        try {
            super.doStart();
            startCamelInThreadContext();
            updateJBIEndpointBinding();
        } catch (Exception ex) {
            RuntimeHelper.logError(ex);
        }
    }

    @Override
    public void doStop() throws DeploymentException {
        try {
            if (this.mCamelMainObj != null && mCamelMainClazz != null) {
                Method stopMethod = mCamelMainClazz.getMethod("stop", null);
                stopMethod.invoke(mCamelMainObj, null);
                //this.mCamelMain.stop();
            }
        } catch (Exception ex) {
            RuntimeHelper.logDebug(ex);
        }
        super.doStop();
    }

    private void updateJBIEndpointBinding() {
        RuntimeHelper.getLogger().fine("### Updating JBIEndpoint Binding for Consumers");
        Collection<ConsumerEndpoint> list = this.getConsumerEndpoints();
        for (ConsumerEndpoint jbiConsumerEP : list) {
            QName servQName = jbiConsumerEP.getService().getServiceName();
            String epName = jbiConsumerEP.getService().getEndpointName();
            List<CamelContext> ctxList = getCamelContexts();
            for (CamelContext ctx : ctxList) {
                HashMap<String,JBIBridgeEndpoint> epMap =  JBIBridgeComponent.findEndpoints(ctx, servQName, epName);
                for (JBIBridgeEndpoint jbiBridgeEP : epMap.values() ) {
                    if ( jbiBridgeEP != null ) {
                        RuntimeHelper.getLogger().fine("Updating JBIEndpoint for camelEP " + jbiBridgeEP.getEndpointUri());
                        jbiBridgeEP.setJbiConsumerEP((CamelSEConsumerEndpoint)jbiConsumerEP);
                    }
                }
            }
        }
    }

    public List<CamelContext> getCamelContexts() {
        List<CamelContext> ctxList = new ArrayList<CamelContext>();
        try {
            Method getCamelContextsMethod = mCamelMainClazz.getMethod("getCamelContexts", null);
            List<CamelContext> springCCtxList = (List<CamelContext>)getCamelContextsMethod.invoke(mCamelMainObj, null);
            //List<CamelContext> springCCtxList = this.mCamelMain.getCamelContexts();
            ctxList.addAll(springCCtxList);
        } catch (Exception ex) {
            RuntimeHelper.getLogger().log(Level.SEVERE, null, ex);
        } 
        return ctxList;
    }

    public JBIBridgeEndpoint getCamelEndpoint(QName servQName, String epName, String operation) {
        JBIBridgeEndpoint camelEP = null;

        List<CamelContext> ctxList = getCamelContexts();
        // RuntimeHelper.logDebug("Searching for JBI Camel Endpoint with URI " + epURI);

        for (CamelContext ctx : ctxList) {
            RuntimeHelper.logDebug("Searching for JBI Camel EP in CamelContext " + ctx.getName());
            camelEP = JBIBridgeComponent.findEndpoint(ctx, servQName, epName, operation);
            if (camelEP != null) {
                RuntimeHelper.logDebug("Found CamelEP");
                break;
            }
        }
        return camelEP;
    }

    public JBIBridgeEndpoint getCamelEndpoint(MessageExchange me) {
        QName servQName = me.getEndpoint().getServiceName();
        String epName = me.getEndpoint().getEndpointName();
        QName opQName = me.getOperation();
        String operation = null;
        if ( opQName != null ) {
            operation = opQName.getLocalPart();
        }
        JBIBridgeEndpoint ep = null;
        ep = getCamelEndpoint(servQName, epName, operation);
        if ( ep == null) {
            // try the default endpoint with operation=null;
            ep = getCamelEndpoint(servQName, epName, null);
        }
        return ep;
    }
}
