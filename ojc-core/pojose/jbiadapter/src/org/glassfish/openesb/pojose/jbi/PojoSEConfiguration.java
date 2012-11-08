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
 * @(#)PojoSEConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import com.sun.jbi.common.qos.config.AbstractConfigMBean;
import com.sun.jbi.common.qos.config.ComponentConfig;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.management.AttributeChangeNotification;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanNotificationInfo;
import javax.management.Notification;
import javax.management.NotificationBroadcasterSupport;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;

/**
 *
 * @author gpatil
 */
public class PojoSEConfiguration  extends AbstractConfigMBean
                        implements PojoSEConfigurationMBean {

    private NotificationBroadcasterSupport broadcasterSupport = new NotificationBroadcasterSupport();    
    
    public PojoSEConfiguration(ComponentContext ctx, ComponentConfig config) 
                                    throws DeploymentException {
        super(ctx, config);
    }

    public Integer getCoreThreadPoolSize() {
        String tc = getConfig().getProperty(CORE_THREAD_POOL_SIZE).getValue();
        Integer ret = new Integer(DEFAULT_CORE_THREAD_POOL_SIZE);
        if (tc != null){
            try {
                ret = Integer.valueOf(tc);
            } catch (Exception ex){
                ret = new Integer(DEFAULT_CORE_THREAD_POOL_SIZE);
            }
        }
        return ret;
    }

    public Integer getMaxThreadPoolSize() {
        String tc = getConfig().getProperty(MAX_THREAD_POOL_SIZE).getValue();
        Integer ret = new Integer(DEFAULT_MAX_THREAD_POOL_SIZE);
        if (tc != null){
            try {
                ret = Integer.valueOf(tc);
            } catch (Exception ex){
                ret = new Integer(DEFAULT_MAX_THREAD_POOL_SIZE);
            }
        }
        return ret;
    }

    public void setCoreThreadPoolSize(Integer cz) {
        int val = (cz == null) ? DEFAULT_CORE_THREAD_POOL_SIZE : cz.intValue();
        String ov = getConfig().getProperty(CORE_THREAD_POOL_SIZE).getValue();
        getConfig().getProperty(CORE_THREAD_POOL_SIZE).setValue(String.valueOf(val));        
        persistCfg();
        String msg = I18n.loc("POJOSE-4550: Changed core thread pool size from {0} to {1}", ov, val);
        Logger.getLogger(PojoSEConfiguration.class.getName()).config(msg);
        fireCfgEvent(msg, CORE_THREAD_POOL_SIZE, Integer.class.getName(), ov, cz);
    }

    public void setMaxThreadPoolSize(Integer mz) {
        int val = (mz == null) ? DEFAULT_MAX_THREAD_POOL_SIZE : mz.intValue();
        String ov = getConfig().getProperty(MAX_THREAD_POOL_SIZE).getValue();
        getConfig().getProperty(MAX_THREAD_POOL_SIZE).setValue(String.valueOf(val));        
        persistCfg();
        String msg = I18n.loc("POJOSE-4551: Changed max thread pool size from {0} to {1}", ov, val);
        Logger.getLogger(PojoSEConfiguration.class.getName()).config(msg);
        fireCfgEvent(msg, CORE_THREAD_POOL_SIZE, Integer.class.getName(), ov, mz);
    }

    /**
     * 
     * Returns ThreadPoolBlockingQueueSize. Note ThreadPoolBlockingQueueSize can
     * only be set during bootstrap process. Hence the setter is in bootstrap
     * config MBean.
     * 
     * @return Integer ThreadPoolBlockingQueueSize
     */
    public Integer getThreadPoolBlockingQueueSize() {
        String tc = getConfig().getProperty(THREAD_POOL_BLOCKING_QUEUE_SIZE).getValue();
        Integer ret = new Integer(DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE);
        if (tc != null){
            try {
                ret = Integer.valueOf(tc);
            } catch (Exception ex){
                ret = new Integer(DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE);
            }
        }
        return ret;
    }
    
    public void setThreadPoolBlockingQueueSize(Integer cz) {
        int val = (cz == null) ? DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE : cz.intValue();
        String ov = getConfig().getProperty(THREAD_POOL_BLOCKING_QUEUE_SIZE).getValue();
        getConfig().getProperty(THREAD_POOL_BLOCKING_QUEUE_SIZE).setValue(String.valueOf(val));
        persistCfg();
        String msg = I18n.loc("POJOSE-4552: Changed blocking queue size from {0} to {1}", ov, val);
        Logger.getLogger(PojoSEConfiguration.class.getName()).config(msg);
        //fireCfgEvent(msg, CORE_THREAD_POOL_SIZE, Integer.class.getName(), ov, mz);
    }    

    private synchronized void fireCfgEvent(String msg, String attrName, String attrType,
            Object oldVal, Object newVal) {
        long seqNo = 0;
        Notification notif = new AttributeChangeNotification(this, seqNo,
                System.currentTimeMillis(), msg, attrName,
                attrType, oldVal, newVal);
        broadcasterSupport.sendNotification(notif);
    }

    protected void persistCfg(){
        try {
            this.persistConfiguration();
        } catch (Exception ex){
            String msg = I18n.loc("POJOSE-7521: Exception while persisting configuration changes. {0}", ex);
            Logger.getLogger(PojoSEConfiguration.class.getName()).severe(msg);
        }
    }
    // Support notifying about config changes
    public MBeanNotificationInfo[] getNotificationInfo(){
        return new MBeanNotificationInfo[] {new MBeanNotificationInfo(new String[] {AttributeChangeNotification.ATTRIBUTE_CHANGE}, AttributeChangeNotification.class.getName(), "Attribute changed")};
    }

    public void addNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback){
        broadcasterSupport.addNotificationListener(listener, filter, handback);
    }

    public void removeNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
        broadcasterSupport.removeNotificationListener(listener);
    }

    public void removeNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) throws ListenerNotFoundException{
        broadcasterSupport.removeNotificationListener(listener, filter, handback);
    }    
}

