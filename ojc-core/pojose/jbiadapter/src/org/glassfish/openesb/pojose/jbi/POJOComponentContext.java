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
 * @(#)POJOComponentContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.jbi.cfg.CfgEventListener;
import org.glassfish.openesb.pojose.jbi.thread.InboundProcessor;
import org.glassfish.openesb.pojose.jbi.su.PojoSEServiceUnit;

/**
 *
 * @author gpatil
 */
public class POJOComponentContext {
    private ComponentContext jbiComponentContext;
    private PojoSELifeCycle compLifeCycle;
    // TODO We may not need volatile for the below fields.
    private volatile PojoSEServiceUnitManager suManager;
    private volatile MessagingChannel mc = null;
    private ReentrantLock suMngrLock = new ReentrantLock();
    private InboundProcessor inbProcessor;    
    private Map<String, PojoSEServiceUnit> suName2su = new ConcurrentHashMap<String, PojoSEServiceUnit>();
    private Map<ServiceEndpoint, PojoSEServiceUnit> sEpt2su = new ConcurrentHashMap<ServiceEndpoint, PojoSEServiceUnit>();
    private Logger compLogger = Logger.getLogger(PojoSEComponentManager.class.getName());
    private PojoSEConfigurationMBean config = null;
    
    /**
     * Service Engine Component context.
     */
    public POJOComponentContext(){
    }
        
    // ***
    // * Getter & Setters
    // ***
    public PojoSELifeCycle getCompLifeCycle() {
        if (compLifeCycle == null){
             if (compLogger.isLoggable(Level.FINE)) {
                String msg = I18n.lf("POJOSE-1500: Instantiating PojoSELifeCycle.");//NOI18N
                compLogger.fine(msg);
             }
            this.compLifeCycle = new PojoSELifeCycle(this);
        }
        
        return compLifeCycle;
    }

    public void setConfigMbean(PojoSEConfigurationMBean config){
        this.config = config;
    }

    public PojoSEConfigurationMBean getConfigMbean(){
        return this.config;
    }
    
    public void setJBIComponentContext(ComponentContext cc){
        this.jbiComponentContext = cc;
    }

    public ComponentContext getJBIComponentContext(){
        return this.jbiComponentContext;
    }
    
    public Logger getComponentLogger() {
        return compLogger;
    }

    public synchronized MessagingChannel getDC() throws MessagingException {
        if (this.mc == null){
            this.mc = mc = new BaseMessagingChannel(this.jbiComponentContext);
        }
        return this.mc;
    }

    public PojoSEServiceUnitManager getServiceUnitManager(){
        if (this.suManager == null){
            try {
                suMngrLock.lock();
                if (this.suManager == null){
                    this.suManager = new PojoSEServiceUnitManager(this);
                }
            }finally {
                suMngrLock.unlock();
            }
        }

        return this.suManager;
    }
    
    public PojoSEServiceUnit getPojoSU(String suName){
        return this.suName2su.get(suName);
    }

    /**
     * Returns PojoSEServiceUnit
     * @param sept Provider internal/logical endpoint.
     * @return PojoSEServiceUnit
     */
    public PojoSEServiceUnit getPojoSU(ServiceEndpoint sept){
        return this.sEpt2su.get(sept);
    }
    
    public void putPojoSU(String suName, PojoSEServiceUnit su){
        this.suName2su.put(suName, su);
    }

    public void putPojoSU(ServiceEndpoint sept, PojoSEServiceUnit su){
        this.sEpt2su.put(sept, su);
    }

    public void removePojoSU(ServiceEndpoint sept){
        this.sEpt2su.remove(sept);
    }
    
    // ***
    // * JBI Component level operations.
    // ***

    /**
     * Start listening to NMR/DC
     */
    public void startComponent(){
        if (inbProcessor == null){
            inbProcessor = InboundProcessor.getInstance(this);
            this.config.addNotificationListener(inbProcessor, null, null);
        }
        
        inbProcessor.startProcessor();
        
        String[] msg = I18n.locStr("POJOSE-5505: POJO SE Component started.");
        this.compLogger.info(msg[2]);
        I18n.alertInfo(msg);
    }

    /**
     * Stop listening to NMR/DC
     */    
    public void stopComponent(){
        assert inbProcessor != null;
        try {
            inbProcessor.stopProcessor();
            String[] msg = I18n.locStr("POJOSE-5506: POJOSE Component stopped.");
            this.compLogger.info(msg[2]);
            I18n.alertInfo(msg);
        } catch (InterruptedException ex) {
            String msg = I18n.loc("POJOSE-7510: Error while stopping the component. {0}", ex.getMessage());
            this.compLogger.log(Level.SEVERE, msg, ex);   
        }
    }
}
