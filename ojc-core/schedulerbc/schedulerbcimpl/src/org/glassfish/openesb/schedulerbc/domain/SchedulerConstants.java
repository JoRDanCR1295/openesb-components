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
 * @(#)SchedulerConstants.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.component.toolkit.util.ExchangePattern;
import java.text.SimpleDateFormat;
import java.util.regex.Pattern;
import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.schedulerbc.I18n;
import org.quartz.impl.StdSchedulerFactory;

/**
 * Defines often used constants for Scheduler BC.
 * 
 * @author sunsoabi_edwong
 */
public interface SchedulerConstants {

    public static final String SCHEDULER_SCHEMA_NAMESPACE =
            "http://schemas.sun.com/jbi/wsdl-extensions/scheduler/";    //NOI18N
    
    public static final String COMPONENT_NAME = "sun-scheduler-binding";//NOI18N
    
    public static final String SCHEDULER_PREFIX = "sched";              //NOI18N
    
    public static final String SCHEDULER_PART_NAME = "schedPart";       //NOI18N
    
    public static final Pattern APP_VAR_PATTERN = Pattern.compile(
            "\\$\\{([^\\$\\{\\}]+)\\}");                                //NOI18N
    
    public static final SimpleDateFormat XSD_STD_SDF =
            new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");         //NOI18N
    
    public enum SchedAttrValue {
        NOW("now", "STR_NOW_VAL"),                                      //NOI18N
        NEVER("never", "STR_NEVER_VAL"),                                //NOI18N
        INDEFINITE("indefinite", "STR_INDEFINITE_VAL"),                 //NOI18N
        SIMPLE("simple", "STR_SIMPLE_VAL"),                             //NOI18N
        CRON("cron", "STR_CRON_VAL"),                                   //NOI18N
        HYBRID("hybrid", "STR_HYBRID_VAL"),                             //NOI18N
        NTH_INCLUDED_DAY("NthIncludedDay", "STR_NTH_INCLUDED_DAY_VAL"), //NOI18N
        STATIC("static", "STR_STATIC_VAL"),                             //NOI18N
        DYNAMIC("dynamic", "STR_DYNAMIC_VAL");                          //NOI18N
        
        private String value;
        private String display;
        SchedAttrValue(String value, String key) {
            this.value = value;
            display = I18n.getString(SchedulerConstants.class, key);
        }
        
        public String getValue() {
            return value;
        }
        
        public String getDisplay() {
            return display;
        }
        
        public static SchedAttrValue toEnum(String s) {
            for (SchedAttrValue v : values()) {
                if (v.getValue().equalsIgnoreCase(s)
                        || v.getDisplay().equals(s)) {
                    return v;
                }
            }
            return null;
        }
    }
    
    public static final String PASSWORD_MASK = "*******";               //NOI18N

    public static final String JOB_SUFFIX = "-job";                     //NOI18N

    public static final String STATIC_JOB_SUFFIX =
            "-" + SchedAttrValue.STATIC.getValue() + JOB_SUFFIX;        //NOI18N

    public static final String DYNAMIC_JOB_SUFFIX =
            "-" + SchedAttrValue.DYNAMIC.getValue() + JOB_SUFFIX;       //NOI18N
    
    public static final String JOB_DESCRIPTION = "Job for:";            //NOI18N
    
    public static final String SERVICE_PORT_SUFFIX = "Port";            //NOI18N
    
    
    public static final String NAME_ATTR_NCNAME = "name";               //NOI18N
    
    public static final String DESCRIPTION_ATTR_NCNAME = "description"; //NOI18N
    
    public static final String MESSAGE_ELEM_NCNAME = "message";         //NOI18N
    
    public static final String PAYLOAD_ELEMENT_NCNAME = "payload";      //NOI18N
    
    public static final String METADATA_ELEM_NCNAME = "metadata";       //NOI18N
    
    public static final String DATE_ATTR_NCNAME = "date";               //NOI18N

    public static final String ACTIVE_PERIODEX_ELEM_NCNAME =
            "active-period";                                            //NOI18N
    
    public static final String STARTING_ATTR_NCNAME = "starting";       //NOI18N
    
    public static final String ENDING_ATTR_NCNAME = "ending";           //NOI18N
    
    public static final String TIMEZONE_ATTR_NCNAME = "timezone";       //NOI18N
    
    public static final String BINDINGEX_ELEM_NCNAME = "binding";       //NOI18N
    
    public static final String GROUP_ATTR_NCNAME = "group";             //NOI18N
    
    public static final String DATEFORMAT_ATTR_NCNAME = "date-format";  //NOI18N
    
    public static final String BINDINGOPERATIONEX_ELEM_NCNAME =
            "operation";                                                //NOI18N
    
    public static final String MODE_ATTR_NCNAME = "mode";               //NOI18N
    
    public static final String TRIGGEREX_ELEM_NCNAME = "trigger";       //NOI18N
    
    public static final String TYPE_ATTR_NCNAME = "type";               //NOI18N
    
    public static final String ENABLED_ATTR_NCNAME = "enabled";         //NOI18N
    
    public static final String REPEAT_ATTR_NCNAME = "repeat";           //NOI18N
    
    public static final String INTERVAL_ATTR_NCNAME = "interval";       //NOI18N
    
    public static final String CRON_EXPR_ATTR_NCNAME = "cron-expr";     //NOI18N
    
    public static final String DURATION_ATTR_NCNAME = "duration";       //NOI18N
    
    public static final String MESSAGE_ATTR_NCNAME = "message";         //NOI18N

    public static final String SEND_SUCCESSFUL = "SEND_SUCCESSFUL";     //NOI18N

    public static final String SEND_FAILED = "SEND_FAILED";             //NOI18N

    public static final String REDELIVERY_NM_PROPS =
            "REDELIVERY_NM_PROPS";                                      //NOI18N

    public static final String REDELIVERY_NM_PROPS_DELIM = "{";         //NOI18N
    
    public static final String JBI_IS_CLUSTERED_SYS_PROP =
            "com.sun.jbi.isClustered";                                  //NOI18N

    public static final String PROP_SCHED_INSTANCE_NAME =
            StdSchedulerFactory.PROP_SCHED_INSTANCE_NAME;
    public static final String PROP_SCHED_INSTANCE_ID =
            StdSchedulerFactory.PROP_SCHED_INSTANCE_ID;
    public static final String AUTO_GENERATE_INSTANCE_ID =
            StdSchedulerFactory.AUTO_GENERATE_INSTANCE_ID;
    public static final String PROP_JOB_STORE_CLASS =
            StdSchedulerFactory.PROP_JOB_STORE_CLASS;
    public static final String PROP_THREAD_POOL_CLASS =
            StdSchedulerFactory.PROP_THREAD_POOL_CLASS;
    public static final String PROP_SCHED_JOB_FACTORY_CLASS =
            StdSchedulerFactory.PROP_SCHED_JOB_FACTORY_CLASS;
    public static final String PROP_JOB_STORE_USE_PROPS =
            StdSchedulerFactory.PROP_JOB_STORE_USE_PROP;
    public static final String PROP_DATASOURCE_PREFIX =
            StdSchedulerFactory.PROP_DATASOURCE_PREFIX;
    public static final String PROP_DATASOURCE_JNDI_URL =
            StdSchedulerFactory.PROP_DATASOURCE_JNDI_URL;

    public static final String SCHEDBC_DSNAME = "SCHEDBC";              //NOI18N
    public static final String PROP_THREAD_POOL_THREAD_COUNT =
            "org.quartz.threadPool.threadCount";                        //NOI18N
    public static final String PROP_THREAD_POOL_THREAD_PRIORITY =
            "org.quartz.threadPool.threadPriority";                     //NOI18N
    public static final String PROP_JOB_STORE_DRIVER_DELEGATE_CLASS =
            "org.quartz.jobStore.driverDelegateClass";                  //NOI18N
    public static final String PROP_JOB_STORE_DATASOURCE =
            "org.quartz.jobStore.dataSource";                           //NOI18N
    public static final String PROP_JOB_STORE_IS_CLUSTERED =
            "org.quartz.jobStore.isClustered";                          //NOI18N
    
    public enum SchedulerNMProperty {
        INBOUND_DATE("org.glassfish.openesb.scheduler.inbound.date"),   //NOI18N
        INBOUND_NAME("org.glassfish.openesb.scheduler.inbound.name"),   //NOI18N
        INBOUND_GROUP(
            "org.glassfish.openesb.scheduler.inbound.group"),           //NOI18N
        OUTBOUND_GROUP(
            "org.glassfish.openesb.scheduler.outbound.group"),          //NOI18N
        OUTBOUND_DATEFORMAT(
            "org.glassfish.openesb.scheduler.outbound.dateformat"),     //NOI18N
        OUTBOUND_MODE(
            "org.glassfish.openesb.scheduler.outbound.mode"),           //NOI18N
        OUTBOUND_NAME(
            "org.glassfish.openesb.scheduler.outbound.name"),           //NOI18N
        OUTBOUND_TYPE(
            "org.glassfish.openesb.scheduler.outbound.type"),           //NOI18N
        OUTBOUND_ENABLED(
            "org.glassfish.openesb.scheduler.outbound.enabled"),        //NOI18N
        OUTBOUND_DESCRIPTION(
            "org.glassfish.openesb.scheduler.outbound.description"),    //NOI18N
        OUTBOUND_REPEAT(
            "org.glassfish.openesb.scheduler.outbound.repeat"),         //NOI18N
        OUTBOUND_INTERVAL(
            "org.glassfish.openesb.scheduler.outbound.interval"),       //NOI18N
        OUTBOUND_CRONEXPR(
            "org.glassfish.openesb.scheduler.outbound.cronexpr"),       //NOI18N
        OUTBOUND_DURATION(
            "org.glassfish.openesb.scheduler.outbound.duration"),       //NOI18N
        OUTBOUND_STARTING(
            "org.glassfish.openesb.scheduler.outbound.starting"),       //NOI18N
        OUTBOUND_ENDING(
            "org.glassfish.openesb.scheduler.outbound.ending"),         //NOI18N
        OUTBOUND_TIMEZONE(
            "org.glassfish.openesb.scheduler.outbound.timezone");       //NOI18N
        
        private String key;
        private Class type;
        
        SchedulerNMProperty(String key) {
            this(key, String.class);
        }
        
        SchedulerNMProperty(String key, Class type) {
            this.key = key;
            this.type = type;
        }
        
        public String getKey() {
            return key;
        }
        
        public Class getType() {
            return type;
        }
    }

    public enum PatternRole {
        IN_ONLY_CONSUMER(ExchangePattern.IN_ONLY,
                MessageExchange.Role.CONSUMER);

        private ExchangePattern patt;
        private MessageExchange.Role role;

        PatternRole(ExchangePattern patt, MessageExchange.Role role) {
            this.patt = patt;
            this.role = role;
        }

        public ExchangePattern getPattern() {
            return patt;
        }

        public MessageExchange.Role getRole() {
            return role;
        }
        
        public static PatternRole toEnum(MessageExchange me) {
            ExchangePattern p = ExchangePattern.valueOf(me);
            for (PatternRole pr : values()) {
                if (pr.getPattern() == p) {
                    if (pr.getRole().equals(me.getRole())) {
                        return pr;
                    }
                }
            }
            return null;
        }
    }
}
