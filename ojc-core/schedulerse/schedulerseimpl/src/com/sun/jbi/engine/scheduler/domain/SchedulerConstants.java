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

package com.sun.jbi.engine.scheduler.domain;

/**
 * Defines often used constants for Scheduler SE.
 * 
 * @author sunsoabi_edwong
 */
public interface SchedulerConstants {

    public static final String SCHEDULER_SCHEMA_NAMESPACE =
            "http://schemas.sun.com/jbi/engines/scheduler";             //NOI18N
    
    public static final String SCHEDULER_PREFIX = "sched";              //NOI18N
    
    public static final String SCHEDULE_ELEM_NCNAME = "schedule";       //NOI18N
    
    public static final String NAME_ATTR_NCNAME = "name";               //NOI18N
    
    public static final String GROUP_ATTR_NCNAME = "group";             //NOI18N
    
    public static final String ENABLED_ATTR_NCNAME = "enabled";         //NOI18N
    
    public static final String DESCRIPTION_ATTR_NCNAME = "description"; //NOI18N
    
    public static final String TRIGGER_ELEM_NCNAME = "trigger";         //NOI18N
    
    public static final String SIMPLETRIGGER_ELEM_NCNAME =
            "simpleTrigger";                                            //NOI18N
    
    public static final String CRONTRIGGER_ELEM_NCNAME =
            "cronTrigger";                                              //NOI18N
    
    public static final String STARTING_ELEM_NCNAME = "starting";       //NOI18N
    
    public static final String DATE_ELEM_NCNAME = "date";               //NOI18N
    
    public static final String FORMAT_ATTR_NCNAME = "format";           //NOI18N
    
    public static final String VALUE_ATTR_NCNAME = "value";             //NOI18N
    
    public static final String ENDING_ELEM_NCNAME = "ending";           //NOI18N
    
    public static final String REPEAT_ELEM_NCNAME = "repeat";           //NOI18N
    
    public static final String COUNT_ATTR_NCNAME = "count";             //NOI18N
    
    public static final String INDEFINITE_VAL = "indefinite";           //NOI18N
    
    public static final String INTERVAL_ELEM_NCNAME = "interval";       //NOI18N
    
    public static final String WEEKS_ATTR_NCNAME = "weeks";             //NOI18N
    
    public static final String DAYS_ATTR_NCNAME = "days";               //NOI18N
    
    public static final String HOURS_ATTR_NCNAME = "hours";             //NOI18N
    
    public static final String MINUTES_ATTR_NCNAME = "minutes";         //NOI18N
    
    public static final String SECONDS_ATTR_NCNAME = "seconds";         //NOI18N
    
    public static final String MILLISECS_ATTR_NCNAME = "millisecs";     //NOI18N
    
    public static final String NOW_VAL = "now";                         //NOI18N
    
    public static final String NEVER_VAL = "never";                     //NOI18N
    
    public static final String MESSAGE_ELEM_NCNAME = "message";         //NOI18N
    
    public static final String DATEFORMAT_ATTR_NCNAME = "date-format";  //NOI18N
    
    public static final String PAYLOAD_ELEMENT_NCNAME = "payload";      //NOI18N
    
    public static final String METADATA_ELEM_NCNAME = "metadata";       //NOI18N
    
    public static final String DATE_ATTR_NCNAME = "date";               //NOI18N
    
    public static final String JOB_SUFFIX = "-job";                     //NOI18N
    
    public static final String JOB_DESCRIPTION = "Job for:";            //NOI18N
    
    public static final String ARTIFACT_FILE_EXT = ".sched";            //NOI18N
    
    public static final String SERVICE_PORT_SUFFIX = "Port";            //NOI18N

}
