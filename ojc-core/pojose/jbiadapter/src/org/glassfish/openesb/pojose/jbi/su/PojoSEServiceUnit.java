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
 * @(#)PojoSEServiceUnit.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi.su;

import com.sun.jbi.common.descriptor.EndpointInfo;
import java.io.File;
import java.io.FileFilter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.List;
import java.util.Map;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.anno.processor.Message;
import org.glassfish.openesb.pojose.core.anno.processor.POJOAnnotationProcessor;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyClassLoader;
import org.glassfish.openesb.pojose.core.anno.processor.ProxyFactory;
import org.glassfish.openesb.pojose.core.anno.processor.impl.RuntimeVisitor;
import org.glassfish.openesb.pojose.jbi.I18n;
import org.glassfish.openesb.pojose.core.util.Util;

/**
 *
 * @author gpatil
 */
public class PojoSEServiceUnit {
    private String name;
    private String root;
    private ClassLoader classLoader;
    private List<POJOClassMetadata> pojoMetas;
    private List<EndpointInfo> providers = new ArrayList<EndpointInfo>();;
    private Map<EndpointInfo, ServiceEndpoint> ep2Sept =
            new HashMap<EndpointInfo, ServiceEndpoint>();
    private Map<String, POJOClassMetadata> endp2Pojo =
            new HashMap<String, POJOClassMetadata>();
    private List<Message> errorMessages = null;
    
    public enum Status {deployed, initialized, started, stopped, shutdown, undeployed};
    private Status status = Status.shutdown;

    private static Logger logger = Logger.getLogger(PojoSEServiceUnit.class.getName());
    
    public PojoSEServiceUnit(String name, String root){
        this.name = name;
        this.root = root;
        init();
    }

    private void init(){
        RuntimeVisitor visitor = new RuntimeVisitor();
        ProxyClassLoader pcl = ProxyFactory.getClassLoader(getClassLoader());
        POJOAnnotationProcessor.refreshEndpoints(this.root, pcl, visitor);
        List<Message> msgs = visitor.getMessages(Message.MessageType.error);
        if ((msgs != null) && (msgs.size() > 0)){
            this.errorMessages = msgs;
        } else {
            pojoMetas = visitor.getPOJOClassMetadata();
        }
        
        if (pojoMetas != null){
            EndpointInfo ei;

            for (POJOClassMetadata pojom: pojoMetas){
                ei = Util.getEndpointInfo(pojom);
                String key = ((ei.getServiceName() == null) ? "" : ei.getServiceName().toString()) //NOI18N
                        + ((ei.getEndpointName() == null) ? "" : ei.getEndpointName()); //NOI18N
                endp2Pojo.put(key, pojom);
                providers.add(ei);
            }
        }
    }
    
    public boolean hasDeployErrors(){
        boolean ret = false;
        if ((this.errorMessages != null) && (this.errorMessages.size() > 0)){
            ret = true;
        }
        return ret;
    }
    
    public List<Message> getErrorMessages(){
        return Collections.unmodifiableList(this.errorMessages);
    }
    
    public synchronized ClassLoader getClassLoader() {
        if (this.classLoader == null){
            File flRoot = new File(this.root);
            
            FileFilter ff = new FileFilter() {
                public boolean accept(File pathname) {
                    if (pathname.getName().endsWith(".jar")) { //NOI18N
                        return true;
                    }
                    return false;
                }
            };

            try {
                List<URL> libs = new ArrayList<URL>();
                if (logger.isLoggable(Level.FINE)) {
                    String msg = I18n.lf("POJOSE-1514: Adding {0} to the SU classpath.", flRoot.getName()); //NOI18N
                    logger.fine(msg);
                }
                libs.add(flRoot.toURL());
                libs.addAll(getLibURLs(flRoot, ff));                
                File libDir = new File(flRoot, "lib"); //NOI18N
                if (libDir.exists() && libDir.isDirectory()){
                    libs.addAll(getLibURLs(libDir, ff));
                }
                URL[] urls = libs.toArray(new URL[0]);
                this.classLoader = new SUClassLoader(urls, this.getClass().getClassLoader());
            } catch (MalformedURLException ex) {
                Logger.getLogger(PojoSEServiceUnit.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return classLoader;
    }
    
    public String getName() {
        return name;
    }

    public List<EndpointInfo> getProviderEps() {
        return this.providers;
    }

    public void addServiceEndpoint(EndpointInfo ei, ServiceEndpoint sept){
        this.ep2Sept.put(ei, sept);
    }

    public ServiceEndpoint getServiceEndpoint(EndpointInfo ei){
        return this.ep2Sept.get(ei);
    }

    public String getRoot() {
        return root;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    public List<POJOClassMetadata> getPojoClassMetadatas(){
        return Collections.unmodifiableList(pojoMetas);
    }
    
    public POJOClassMetadata getPojoClassMetadata(String key){
        return endp2Pojo.get(key);
    }

    public POJOClassMetadata getPojoClassMetadata(ServiceEndpoint sept){
        String key = ((sept.getServiceName() == null) ?
            "" : sept.getServiceName().toString())
            + ((sept.getEndpointName() == null) ? "" : sept.getEndpointName());

        return endp2Pojo.get(key);
    }

    public POJOClassMetadata getPojoClassMetadata(EndpointInfo ei){
        String key = ((ei.getServiceName() == null) ? "" :
            ei.getServiceName().toString()) + ((ei.getEndpointName() == null) ?
                "" : ei.getEndpointName()) ;
        return endp2Pojo.get(key);
    }
    
    private List<URL> getLibURLs(File root, FileFilter ff){
        List<URL> ret = new ArrayList<URL>();
        File[] libs = root.listFiles(ff);
        for (File lib: libs){
            try {
                ret.add(lib.toURL());

                if (logger.isLoggable(Level.FINE)) {
                    String msg = I18n.lf("POJOSE-1514: Adding {0} to the SU classpath.", lib.getName());//NOI18N
                    logger.fine(msg);
                }
            } catch (MalformedURLException ex) {
                Logger.getLogger(PojoSEServiceUnit.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
       
        return ret;
    }    
}
