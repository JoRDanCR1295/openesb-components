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


package com.sun.jbi.mm;

import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.xml.xpath.XPathExpression;
import com.sun.jbi.mm.CustomConfiguration.MonitorConfig.Component;
import com.sun.jbi.mm.CustomConfiguration.PayloadEnum;


/**
 * PIX/PDQ Management Monitoring Utility.
 *
 * @author ylee
 * @author sunsoabi_edwong
 */
public class MMUtil {

    public static final String SOLUTION_GROUP_KEY =
            "monitor.solutiongroup";                                    //NOI18N
    public static final String MONITOR_ENABLED_SUFFIX_KEY =
            "monitor.enabled";                                          //NOI18N
    public static final String KEY_SEPARATOR = ".";                     //NOI18N
    public static final String SOLUTION_GROUP = "solutiongroup";        //NOI18N
    public static final String PUSH_NDC_GATHER = "com.sun.EnterContext";//NOI18N
    public static final String POP_NDC_GATHER = "com.sun.ExitContext";  //NOI18N
    public static final String GFESBCUSTOM_PROPS_KEY =
            "com.sun.esb.custom.properties.file";                       //NOI18N

    // checkpoint defines
    public static final String CHECKPOINT_LOGGER =
            "com.sun.monitor.checkpoint.Set";                           //NOI18N
    public static final String MSG_ID_KEY = "MSG-ID";                   //NOI18N
    public static final String COMPONENT_ID_KEY = "COMPONENT-ID";       //NOI18N
    public static final String SERVICE_INSTANCE_ID_KEY =
            "SERVICE-INSTANCE-ID";                                      //NOI18N
    public static final String CHECKPOINT_TIMESTAMP_KEY =
            "CHECKPOINT-TIMESTAMP";                                     //NOI18N
    public static final String DESCRIPTION_KEY = "DESCRIPTION";         //NOI18N
    public static final String PAYLOAD_KEY = "PAYLOAD";                 //NOI18N
    public static final String EXTENSIONS_KEY = "EXTENSIONS";           //NOI18N

    // message tracking ID modes
    public static final String MESSAGE_TRACKING_MODE_MSG = "msg";       //NOI18N
    public static final String MESSAGE_TRACKING_MODE_AUTO = "auto";     //NOI18N

    // message tracking ID key
    public static final String MESSAGE_TRACKING_ID_KEY =
            "org.glassfish.openesb.messaging.messageid";                //NOI18N
    public static final String MESSAGE_TRACKING_GROUP_ID_KEY =
            "org.glassfish.openesb.messaging.groupid";                  //NOI18N

    protected static CustomConfiguration config = null;

    public static final String SERVER_MBEAN_NAME =
            "com.sun.ihe:type=sunpixpdq,name=server";                   //NOI18N

    private static Logger logger = Logger.getLogger(MMUtil.class.getName());

    /**
     * Gets the Custom Configuration object from cache.
     * @return Custom Configuration object.
     */
    public static CustomConfiguration getCustomConfiguration() {
        if (config == null) {
            // get custom configuration path from system property
            String configFilePath = System.getProperty(GFESBCUSTOM_PROPS_KEY);
            config = new CustomConfiguration(configFilePath);
        }
        return config;
    }

    /**
     * Gets the Custom Configuration object from cache with option to refresh.
     * @param refresh <code>true</code> if cache is refreshed first.
     * @return Custom Configuration object.
     */
    public static CustomConfiguration getCustomConfiguration(boolean refresh) {
        if ( refresh ) {
            return refreshConfiguration();
        } else {
            return getCustomConfiguration();
        }
    }

    private static MBeanServerConnection serverConnection = null;
    private static boolean mMBeanNotRegistered;

    /**
     * Get refreshed Custom Configuration object.
     * @return Custom Configuration object.
     */
    public static CustomConfiguration refreshConfiguration() {

        if (config == null) {
            config = getCustomConfiguration();
        } else {
            if (mMBeanNotRegistered) {
                return config;
            }
            
            // get a fresh copy from mbean
            try {
                if (serverConnection == null) {
                    serverConnection =
                            ManagementFactory.getPlatformMBeanServer();
                }
                Properties props = (Properties) serverConnection.invoke(
                        new ObjectName(SERVER_MBEAN_NAME),
                        "getConfiguration", null, null);                //NOI18N
                config.refreshConfig(props);
            } catch (Exception e) {
                mMBeanNotRegistered = true;
                logger.warning("Unable to find server mbean: "          //NOI18N
                        + SERVER_MBEAN_NAME
                        + ". MBean is not registered." );               //NOI18N
            }
        }
        return config;
    }

    /**
     * return the raw configuration properties
     * @return
     */
    public static Properties getConfigProperties() {
        return getCustomConfiguration().getConfigProperties();
    }

    /**
     * get a list of operation names
     * @param componentName Name of the componnent.
     * @param serviceInstanceId Service instance ID.
     * @return
     */
    public static List<String> getOperationNames(String componentName,
            String serviceInstanceId) {
        return getCustomConfiguration().getMonitorConfig().getOperationNames(
                componentName, serviceInstanceId);
    }

    /**
     * Gets a unique ID.
     * @return Unique ID.
     */
    public static String getUniqueID() {
        return UUID.randomUUID().toString();
    }

    /**
     * Gets the checkpoint monitor solution group.
     * @param componentId ID of the component.
     * @param serviceInstanceId Service instance ID.
     * @return
     */
    public static String getSolutionGroup(String componentId,
            String serviceInstanceId) {
        return getCustomConfiguration().getMonitorConfig().getSolutionGroup(
                componentId, serviceInstanceId);
    }

    /**
     * Tests if NDC gather is enabled.
     * @param componentName Name of the component.
     * @param serviceInstanceId Service instance ID.
     * @return <code>true</code> if so.
     */
    public static boolean isNdcEnabled(String componentName,
            String serviceInstanceId) {
        return getCustomConfiguration().getNdcConfig().isNdcEnabled(
                componentName, serviceInstanceId);
    }

    /**
     * Tests if checkpoint monitoring is enabled.
     * @param componentName Name of the component.
     * @param serviceInstanceId Service instance ID.
     * @return <code>true</code> if so.
     */
    public static boolean isMonitorEnabled(String componentName,
            String serviceInstanceId) {
        return getCustomConfiguration().getMonitorConfig().isMonitorEnabled(
                componentName, serviceInstanceId);
    }

    /**
     * Gets the inbound message tracking ID generation mode.
     * @return Mode of determine inbound message tracking ID.
     */
    public static String getMessageTrackingIDModeInbound() {
        return getCustomConfiguration().getMonitorConfig()
                .getMessageTrackingIDModeInbound();
    }

    /**
     * Gets the XPathExpression that will generate the message tracking ID.
     * @param componentId ID of component.
     * @param serviceInstanceId Service instance ID.
     * @param operation Operation being called.
     * @return
     */
    public static XPathExpression getMessageTrackingXPathExpression(
            String componentId, String serviceInstanceId, String operation) {
        return getCustomConfiguration().getMonitorConfig()
                .getMessageTrackingXPathExpression(componentId,
                        serviceInstanceId, operation);
    }

    
    ///////////////////////////////////////////////////////////////////////////
    //
    // checkpoint methods
    //
    ///////////////////////////////////////////////////////////////////////////

    /**
     * set a checkpoint
     * @param solutionGroup - name of the solution group e.g SUNPIXPDQ
     * @param msgId         - tracking id
     * @param componentId   - name of component e.g. sun-hl7-binding
     * @param serviceInstanceId - service endpoint qname
     * @param desc          - annotation for the checkpoint
     * @param timestamp     - time this checkpoint is recorded
     */
    public static void setCheckpoint(String solutionGroup, String msgId,
            String componentId, String serviceInstanceId, String desc,
            Date timestamp) {
        setCheckpoint(solutionGroup, msgId, componentId, serviceInstanceId,
                desc, timestamp, null, null);
    }

    /**
     * set a checkpoint
     * @param solutionGroup - name of the solution group e.g SUNPIXPDQ
     * @param msgId         - tracking id
     * @param componentId   - name of component e.g. sun-hl7-binding
     * @param serviceInstanceId - service endpoint qname
     * @param desc          - annotation for the checkpoint
     * @param timestamp     - time this checkpoint is recorded
     * @param payload       - payload - supports String and Document types
     */
    public static void setCheckpoint(String solutionGroup, String msgId,
            String componentId, String serviceInstanceId, String desc,
            Date timestamp, Object payload) {
        setCheckpoint(solutionGroup, msgId, componentId, serviceInstanceId,
                desc, timestamp, payload, null);
    }

    /**
     * set a checkpoint
     * @param solutionGroup - name of the solution group e.g SUNPIXPDQ
     * @param msgId         - tracking id
     * @param componentId   - name of component e.g. sun-hl7-binding
     * @param serviceInstanceId - service endpoint qname
     * @param desc          - annotation for the checkpoint
     * @param extensions    - any other info that need to be stored with the
     *                        checkpoint
     *                        - format: key1=val1[,keyn=valn]
     *                        -   e.g.  bpel1d=1
     * @param timestamp     - time this checkpoint is recorded
     */
    public static void setCheckpoint(String solutionGroup, String msgId,
            String componentId, String serviceInstanceId, String desc,
            Object extensions, Date timestamp) {
        setCheckpoint(solutionGroup, msgId, componentId, serviceInstanceId,
                desc, timestamp, null, extensions);
    }

    /**
     * set a checkpoint
     * @param solutionGroup - name of the solution group e.g SUNPIXPDQ
     * @param msgId         - tracking id
     * @param componentId   - name of component e.g. sun-hl7-binding
     * @param serviceInstanceId - service endpoint qname
     * @param desc          - annotation for the checkpoint
     * @param timestamp     - time this checkpoint is recorded
     * @param payload       - payload - supports String and Document types
     * @param extensions    - any other info that need to be stored with the
     *                        checkpoint
     *                        - format: key1=val1[,keyn=valn]
     *                        -   e.g.  bpel1d=1
     */
    public static void setCheckpoint(String solutionGroup, String msgId,
            String componentId, String serviceInstanceId, String desc,
            Date timestamp, Object payload, Object extensions) {
        ArrayList objList = new ArrayList(5);

        objList.add(MSG_ID_KEY);
        objList.add(msgId);

        objList.add(COMPONENT_ID_KEY);
        objList.add(componentId);

        objList.add(SERVICE_INSTANCE_ID_KEY);
        objList.add(serviceInstanceId);

        objList.add(CHECKPOINT_TIMESTAMP_KEY);
        objList.add(timestamp);

        objList.add(DESCRIPTION_KEY);
        objList.add(desc);

        if (payload != null) {
            objList.add(PAYLOAD_KEY);
            objList.add(payload);
        }

        if (extensions != null) {
            objList.add(EXTENSIONS_KEY);
            objList.add(extensions);
        }

        Logger.getLogger(CHECKPOINT_LOGGER).log(
                Level.FINE, solutionGroup, objList.toArray());
    }

    /**
     * Gets the send payload action for the respective checkpointing.
     *
     * @param componentName Name of the component, ex. sun-hl7-binding.
     * @param serviceInstanceId Service instance ID of the endpoint.
     * @return <code>PayloadEnum</code> enumeration:
     * <ul>
     * <li><code>YES</code> means yes, always send payload</li>
     * <li><code>NO</code> means no, never send payload</li>
     * <li><code>DIFF</code> means send payload if outgoing payload is different
     * from incoming</li>
     * <li><code>SELF</code> means determine whether to do so according to
     * component's internal logic</li>
     * </ul>
     */
    public static PayloadEnum getPayloadAction(
            String componentName, String serviceInstanceId) {
        String solutionGroup =
                getSolutionGroup(componentName, serviceInstanceId);
        Map<String, Component> comps =
                getCustomConfiguration().getMonitorConfig().getGroupComponents()
                        .get(solutionGroup);
        if (comps != null) {
            for (Component component : comps.values()) {
                if (componentName.equals(component.getComponentid())
                        && serviceInstanceId.equals(
                                component.getServiceInstanceID())) {
                    return component.getPayload();
                }
            }
        }
        return PayloadEnum.SELF;
    }

    /**
     * Push on NDC gather.
     * @param componentName Name of component.
     * @param serviceInstanceId Service instance ID.
     */
    public static void pushNdcGather(String componentName,
            String serviceInstanceId) {
        Logger.getLogger(PUSH_NDC_GATHER).log(Level.FINE, "{0}={1}",    //NOI18N
                new Object[] {
                    SOLUTION_GROUP,
                    getCustomConfiguration().getNdcConfig().getSolutionGroup(
                            componentName, serviceInstanceId)
                });
    }

    /**
     * Pop off NDC gather.
     * @param componentName Name of component.
     * @param serviceInstanceId Service instance ID.
     */
    public static void popNdcGather(String componentName,
            String serviceInstanceId) {
        Logger.getLogger(POP_NDC_GATHER).log(Level.FINE, "{0}={1}",     //NOI18N
                new Object[] {
                    SOLUTION_GROUP,
                    getCustomConfiguration().getNdcConfig().getSolutionGroup(
                            componentName, serviceInstanceId)
                });
    }
}
