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
 * @(#)SchedulerModel.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.scheduler.domain;

import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

/**
 * Models the Scheduler project artifact.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerModel implements SchedulerConstants {    
    
    private static final long SEC_TO_MILLISEC = 1000L;
    private static final long MINUTE_TO_MILLISEC = 60 * SEC_TO_MILLISEC;
    private static final long HOUR_TO_MILLISEC = 60 * MINUTE_TO_MILLISEC;
    private static final long DAY_TO_MILLISEC = 24 * HOUR_TO_MILLISEC;
    private static final long WEEK_TO_MILLISEC = 7 * DAY_TO_MILLISEC;
    private static final String XMLNS = "xmlns";                        //NOI18N
    private static final String XMLNS_PRE = XMLNS + ":";                //NOI18N
    
    private Map<String, String> xmlNamespaces = new HashMap<String, String>();
    private String name;
    private String group;
    private List<Trigger> triggers = new ArrayList<Trigger>();
    
    public SchedulerModel() {
        super();
    }
    
    public Map getXmlNamespaces() {
        return xmlNamespaces;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    
    public String getGroup() {
        return group;
    }
    
    public void setGroup(String group) {
        this.group = group;
    }

    public List<Trigger> getTriggers() {
        return triggers;
    }
    
    public void addTrigger(Trigger trigger) {
        triggers.add(trigger);
    }
    
    public boolean removeTrigger(Trigger trigger) {
        return triggers.remove(trigger);
    }
    
    public class Trigger {
        
        private String name;
        private String group;
        private boolean enabled;
        private String description;
        private SimpleTrigger simpleTrigger;
        private Message message;
        
        public Trigger() {
            super();
        }
        
        public String getName() {
            return name;
        }
        
        public void setName(String name) {
            this.name = name;
        }
        
        public String getGroup() {
            return group;
        }
        
        public void setGroup(String group) {
            this.group = group;
        }
        
        public boolean isEnabled() {
            return enabled;
        }
        
        public void setEnabled(boolean enabled) {
            this.enabled = enabled;
        }
        
        public String getDescription() {
            return description;
        }
        
        public void setDescription(String description) {
            this.description = description;
        }
        
        public SimpleTrigger getSimpleTrigger() {
            return simpleTrigger;
        }
        
        public void setSimpleTrigger(SimpleTrigger simpleTrigger) {
            this.simpleTrigger = simpleTrigger;
        }
        
        public Message getMessage() {
            return message;
        }
        
        public void setMessage(Message message) {
            this.message = message;
        }

        private SimpleTrigger readSimpleTrigger(Element simpleTriggerElem) {
            SimpleTrigger simpTrigger = new SimpleTrigger();
            NodeList kids = simpleTriggerElem.getChildNodes();
            for (int i = 0; i < kids.getLength(); i++) {
                Node node = kids.item(i);
                if (!isSchedulerElement(node)) {
                    continue;
                }
                
                Element kidElem = (Element) node;
                if (STARTING_ELEM_NCNAME.equals(kidElem.getLocalName())) {
                    NodeList elems = getChildSchedulerElements(kidElem,
                            DATE_ELEM_NCNAME);
                    if (elems.getLength() > 0) {
                        Starting starting = new Starting();
                        simpTrigger.setStarting(starting);
                        Element dateElem = (Element) elems.item(0);
                        Date date = new Date(true);
                        starting.setDate(date);
                        date.setFormat(getSchedulerAttribute(dateElem,
                                FORMAT_ATTR_NCNAME));
                        date.setValue(getSchedulerAttribute(dateElem,
                                VALUE_ATTR_NCNAME));
                    }
                } else if (ENDING_ELEM_NCNAME.equals(kidElem.getLocalName())) {
                    NodeList elems = getChildSchedulerElements(kidElem,
                            DATE_ELEM_NCNAME);
                    if (elems.getLength() > 0) {
                        Ending ending = new Ending();
                        simpTrigger.setEnding(ending);
                        Element dateElem = (Element) elems.item(0);
                        Date date = new Date(false);
                        ending.setDate(date);
                        date.setFormat(getSchedulerAttribute(dateElem,
                                FORMAT_ATTR_NCNAME));
                        date.setValue(getSchedulerAttribute(dateElem,
                                VALUE_ATTR_NCNAME));
                    }
                } else if (REPEAT_ELEM_NCNAME.equals(kidElem.getLocalName())) {
                    Repeat repeat = new Repeat();
                    simpTrigger.setRepeat(repeat);
                    repeat.setCount(getSchedulerAttribute(kidElem,
                            COUNT_ATTR_NCNAME));
                    
                    NodeList kidKids = getChildSchedulerElements(kidElem,
                            INTERVAL_ELEM_NCNAME);
                    for (int j = 0; j < kidKids.getLength(); j++) {
                        Element intervalElem = (Element) kidKids.item(j);
                        Interval interval = new Interval();
                        repeat.setInterval(interval);
                        interval.setWeeks(getSchedulerAttribute(
                                intervalElem, WEEKS_ATTR_NCNAME));
                        interval.setDays(getSchedulerAttribute(
                                intervalElem, DAYS_ATTR_NCNAME));
                        interval.setHours(getSchedulerAttribute(
                                intervalElem, HOURS_ATTR_NCNAME));
                        interval.setMinutes(getSchedulerAttribute(
                                intervalElem, MINUTES_ATTR_NCNAME));
                        interval.setSeconds(getSchedulerAttribute(
                                intervalElem, SECONDS_ATTR_NCNAME));
                        interval.setMillisecs(getSchedulerAttribute(
                                intervalElem, MILLISECS_ATTR_NCNAME));
                        break;
                    }
                }
            }
            return simpTrigger;
        }
        
        public class SimpleTrigger {
            
            private Starting starting;
            private Ending ending;
            private Repeat repeat;
            
            public SimpleTrigger() {
                super();
            }

            public Starting getStarting() {
                return starting;
            }

            public void setStarting(Starting starting) {
                this.starting = starting;
            }

            public Ending getEnding() {
                return ending;
            }

            public void setEnding(Ending ending) {
                this.ending = ending;
            }

            public Repeat getRepeat() {
                return repeat;
            }

            public void setRepeat(Repeat repeat) {
                this.repeat = repeat;
            }
        }
        
        public class Starting {
            
            private Date date;
            
            public Starting() {
                super();
            }

            public Date getDate() {
                return date;
            }

            public void setDate(Date date) {
                this.date = date;
            }
        }
        
        public class Date {
            
            private boolean isStarting;
            private String format;
            private String value;
            
            public Date(boolean isStarting) {
                super();
                this.isStarting = isStarting;
            }

            public String getFormat() {
                return format;
            }

            public void setFormat(String format) {
                this.format = format;
            }

            public String getValue() {
                return value;
            }

            public void setValue(String value) {
                this.value = value;
            }
            
            public java.util.Date valueOf() throws ParseException {
                java.util.Date result = null;
                if (Util.isEmpty(value)) {
                    result = (isStarting ? new java.util.Date() : null);
                } else if (NOW_VAL.equalsIgnoreCase(value)) {
                    result = new java.util.Date();
                } else if (NEVER_VAL.equalsIgnoreCase(value)) {
                    result = (isStarting ? new java.util.Date(Long.MAX_VALUE)
                            : null);
                } else {
                    result = (Util.isEmpty(getFormat())
                            ? new SimpleDateFormat()
                            : new SimpleDateFormat(getFormat()))
                                    .parse(getValue());
                }
                return result;
            }
        }
        
        public class Ending {
            
            private Date date;
            
            public Ending() {
                super();
            }

            public Date getDate() {
                return date;
            }

            public void setDate(Date date) {
                this.date = date;
            }
        }
        
        public class Repeat {
            
            private String count;
            private Interval interval;
            
            public Repeat() {
                super();
            }

            public String getCount() {
                return count;
            }

            public void setCount(String count) {
                this.count = count;
            }
            
            public Interval getInterval() {
                return interval;
            }
            
            public void setInterval(Interval interval) {
                this.interval = interval;
            }
            
            public int valueOf() throws NumberFormatException {
                if (Util.isEmpty(count)
                        || INDEFINITE_VAL.equalsIgnoreCase(count)) {
                    return org.quartz.SimpleTrigger.REPEAT_INDEFINITELY;
                } else {
                    return Integer.parseInt(getCount());
                }
            }
        }
        
        public class Interval {
            
            private String weeks;
            private String days;
            private String hours;
            private String minutes;
            private String seconds;
            private String millisecs;
            
            public Interval() {
                super();
            }

            public String getDays() {
                return days;
            }

            public void setDays(String days) {
                this.days = days;
            }

            public String getHours() {
                return hours;
            }

            public void setHours(String hours) {
                this.hours = hours;
            }

            public String getMillisecs() {
                return millisecs;
            }

            public void setMillisecs(String millisecs) {
                this.millisecs = millisecs;
            }

            public String getMinutes() {
                return minutes;
            }

            public void setMinutes(String minutes) {
                this.minutes = minutes;
            }

            public String getSeconds() {
                return seconds;
            }

            public void setSeconds(String seconds) {
                this.seconds = seconds;
            }

            public String getWeeks() {
                return weeks;
            }

            public void setWeeks(String weeks) {
                this.weeks = weeks;
            }
            
            public long valueOf() throws NumberFormatException {
                long result = 0L;
                if (!Util.isEmpty(getWeeks())) {
                    result += (Long.parseLong(getWeeks()) * WEEK_TO_MILLISEC);
                }
                if (!Util.isEmpty(getDays())) {
                    result += (Long.parseLong(getDays()) * DAY_TO_MILLISEC);
                }
                if (!Util.isEmpty(getHours())) {
                    result += (Long.parseLong(getHours()) * HOUR_TO_MILLISEC);
                }
                if (!Util.isEmpty(getMinutes())) {
                    result += (Long.parseLong(getMinutes())
                            * MINUTE_TO_MILLISEC);
                }
                if (!Util.isEmpty(getSeconds())) {
                    result += (Long.parseLong(getSeconds()) * SEC_TO_MILLISEC);
                }
                if (!Util.isEmpty(getMillisecs())) {
                    result += Long.parseLong(getMillisecs());
                }
                return result;
            }
        }
        
        private Message readMessage(Element messageElem) {
            Message lMessage = new Message();
            lMessage.setDateFormat(getSchedulerAttribute(messageElem,
                    DATEFORMAT_ATTR_NCNAME));
            String lPayload = messageElem.getTextContent();
            if (!Util.isEmpty(lPayload)) {
                lMessage.setPayload(lPayload.trim());
            }
            return lMessage;
        }
        
        public class Message {
            
            private String dateFormat;
            private String payload;
            
            public Message() {
                super();
            }

            public String getDateFormat() {
                return dateFormat;
            }

            public void setDateFormat(String dateFormat) {
                this.dateFormat = dateFormat;
            }

            public String getPayload() {
                return payload;
            }

            public void setPayload(String payload) {
                this.payload = payload;
            }
        }
    }
    
    private static String getSchedulerAttribute(Element elem, String attrName) {
        return elem.getAttribute(attrName);
    }
    
    private static boolean isSchedulerElement(Node node) {
        return ((node instanceof Element) && SCHEDULER_SCHEMA_NAMESPACE
                .equals(((Element) node).getNamespaceURI()));
    }
    
    public static SchedulerModel readXml(File xmlFile) {
        BufferedInputStream bis = null;
        try {
            bis = new BufferedInputStream(new FileInputStream(xmlFile));
            Document domDoc = XmlUtil.readXml(bis);
            domDoc.normalize();
            NodeList rootElems = domDoc.getElementsByTagNameNS(
                    SCHEDULER_SCHEMA_NAMESPACE, SCHEDULE_ELEM_NCNAME);
            if (rootElems.getLength() != 1) {
                return null;
            }
            
            SchedulerModel scheduler = new SchedulerModel();
            Element scheduleElem = (Element) rootElems.item(0);
            scheduler.setName(getSchedulerAttribute(scheduleElem,
                    NAME_ATTR_NCNAME));
            scheduler.setGroup(getSchedulerAttribute(scheduleElem,
                    GROUP_ATTR_NCNAME));
            NamedNodeMap schedulerAtts = scheduleElem.getAttributes();
            for (int i = 0; i < schedulerAtts.getLength(); i++) {
                Node node = schedulerAtts.item(i);
                if (!(node instanceof Attr)) {
                    continue;
                }
                
                Attr attr = (Attr) node;
                if (attr.getName().startsWith(XMLNS)) {
                    if (attr.getName().equals(XMLNS)) {
                        scheduler.getXmlNamespaces().put(XMLNS,
                                attr.getValue());
                    } else if (attr.getName().charAt(5) == ':') {       //NOI18N
                        String prefix = attr.getName().substring(6);
                        scheduler.getXmlNamespaces().put(prefix,
                                attr.getValue());
                    }
                }
            }
            
            NodeList schedulerKids = scheduleElem.getChildNodes();
            for (int i = 0; i < schedulerKids.getLength(); i++) {
                Node kid = schedulerKids.item(i);
                if (!isSchedulerElement(kid)) {
                    continue;
                }
                
                Element kidElem = (Element) kid;
                if (TRIGGER_ELEM_NCNAME.equals(kidElem.getLocalName())) {
                    scheduler.addTrigger(scheduler.readTrigger(kidElem));
                }
            }
            return scheduler;
        } catch (Exception e) {
            
        } finally {
            if (bis != null) {
                try {
                    bis.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }
        return null;
    }

    private Trigger readTrigger(Element triggerElem) {
        Trigger trigger = new Trigger();
        trigger.setName(getSchedulerAttribute(triggerElem,
                NAME_ATTR_NCNAME));
        Element schedulerElem = (Element) triggerElem.getParentNode();
        trigger.setGroup(getSchedulerAttribute(schedulerElem,
                GROUP_ATTR_NCNAME));
        String enabledStr = getSchedulerAttribute(triggerElem,
                ENABLED_ATTR_NCNAME);
        if (!Util.isEmpty(enabledStr)) {
            trigger.setEnabled(Boolean.parseBoolean(enabledStr));
        }
        trigger.setDescription(getSchedulerAttribute(triggerElem,
                DESCRIPTION_ATTR_NCNAME));
        NodeList kids = triggerElem.getChildNodes();
        for (int i = 0; i < kids.getLength(); i++) {
            Node node = kids.item(i);
            if (!isSchedulerElement(node)) {
                continue;
            }
            
            Element kidElem = (Element) node;
            if (SIMPLETRIGGER_ELEM_NCNAME.equals(kidElem.getLocalName())) {
                trigger.setSimpleTrigger(trigger.readSimpleTrigger(kidElem));
            } else if (MESSAGE_ELEM_NCNAME.equals(kidElem.getLocalName())) {
                trigger.setMessage(trigger.readMessage(kidElem));
            }
        }
        return trigger;
    }
    
    private Element createSchedulerElement(Document domDoc, String tag) {
        return domDoc.createElementNS(SCHEDULER_SCHEMA_NAMESPACE, tag);
    }
    
    private NodeList getChildSchedulerElements(Element parent, String tag) {
        return parent.getElementsByTagNameNS(SCHEDULER_SCHEMA_NAMESPACE, tag);
    }
    
    @Override
    public String toString() {
        String result = "";                                             //NOI18N
        Document domDoc = XmlUtil.newDocument();
        Element scheduleElem = createSchedulerElement(domDoc,
                SCHEDULE_ELEM_NCNAME);
        domDoc.setXmlVersion("1.0");                                    //NOI18N
        domDoc.appendChild(scheduleElem);
        scheduleElem.setAttribute(NAME_ATTR_NCNAME, getName());
        scheduleElem.setAttribute(GROUP_ATTR_NCNAME, getGroup());
        for (Map.Entry<String, String> me : xmlNamespaces.entrySet()) {
            if (!Util.isEmpty(me.getKey()) && !Util.isEmpty(me.getValue())) {
                if (XMLNS.equals(me.getKey())) {
                    scheduleElem.setAttribute(XMLNS, me.getValue());
                } else {
                    scheduleElem.setAttribute(XMLNS_PRE + me.getKey(),
                            me.getValue());
                }
            }
        }
        
        for (Trigger trigger : triggers) {
            Element triggerElem = createSchedulerElement(domDoc,
                    TRIGGER_ELEM_NCNAME);
            scheduleElem.appendChild(triggerElem);
            
            if (!Util.isEmpty(trigger.getName())) {
                triggerElem.setAttribute(NAME_ATTR_NCNAME, trigger.getName());
            }
            triggerElem.setAttribute(ENABLED_ATTR_NCNAME,
                    Boolean.toString(trigger.isEnabled()));
            if (!Util.isEmpty(trigger.getDescription())) {
                triggerElem.setAttribute(DESCRIPTION_ATTR_NCNAME,
                        trigger.getDescription());
            }
            
            if (trigger.getSimpleTrigger() != null) {
                Trigger.SimpleTrigger lSimpleTrigger =
                        trigger.getSimpleTrigger();
                Element simpleTriggerElem = createSchedulerElement(domDoc,
                        SIMPLETRIGGER_ELEM_NCNAME);
                triggerElem.appendChild(simpleTriggerElem);
                
                if (lSimpleTrigger.getStarting() != null) {
                    Trigger.Starting lStarting = lSimpleTrigger.getStarting();
                    Element startingElem = createSchedulerElement(domDoc,
                            STARTING_ELEM_NCNAME);
                    simpleTriggerElem.appendChild(startingElem);
                    
                    if (lStarting.getDate() != null) {
                        Trigger.Date lDate = lStarting.getDate();
                        Element dateElem = createSchedulerElement(domDoc,
                                DATE_ELEM_NCNAME);
                        startingElem.appendChild(dateElem);
                        
                        if (!Util.isEmpty(lDate.getFormat())) {
                            dateElem.setAttribute(FORMAT_ATTR_NCNAME,
                                    lDate.getFormat());
                        }
                        if (!Util.isEmpty(lDate.getValue())) {
                            dateElem.setAttribute(VALUE_ATTR_NCNAME,
                                    lDate.getValue());
                        }
                    }
                }
                
                if (lSimpleTrigger.getEnding() != null) {
                    Trigger.Ending lEnding = lSimpleTrigger.getEnding();
                    Element endingElem = createSchedulerElement(domDoc,
                            ENDING_ELEM_NCNAME);
                    simpleTriggerElem.appendChild(endingElem);
                    
                    if (lEnding.getDate() != null) {
                        Trigger.Date lDate = lEnding.getDate();
                        Element dateElem = createSchedulerElement(domDoc,
                                DATE_ELEM_NCNAME);
                        endingElem.appendChild(dateElem);
                        
                        if (!Util.isEmpty(lDate.getFormat())) {
                            dateElem.setAttribute(FORMAT_ATTR_NCNAME,
                                    lDate.getFormat());
                        }
                        if (!Util.isEmpty(lDate.getValue())) {
                            dateElem.setAttribute(VALUE_ATTR_NCNAME,
                                    lDate.getValue());
                        }
                    }
                }
                
                if (lSimpleTrigger.getRepeat() != null) {
                    Element repeatElem = createSchedulerElement(domDoc,
                            REPEAT_ELEM_NCNAME);
                    simpleTriggerElem.appendChild(repeatElem);
                    
                    if (!Util.isEmpty(lSimpleTrigger.getRepeat().getCount())) {
                        repeatElem.setAttribute(COUNT_ATTR_NCNAME,
                                lSimpleTrigger.getRepeat().getCount());
                    }
                
                    if (lSimpleTrigger.getRepeat().getInterval() != null) {
                        Trigger.Interval lInterval =
                                lSimpleTrigger.getRepeat().getInterval();
                        Element intervalElem = createSchedulerElement(domDoc,
                                INTERVAL_ELEM_NCNAME);
                        repeatElem.appendChild(intervalElem);

                        if (!Util.isEmpty(lInterval.getWeeks())) {
                            intervalElem.setAttribute(WEEKS_ATTR_NCNAME,
                                    lInterval.getWeeks());
                        }
                        if (!Util.isEmpty(lInterval.getDays())) {
                            intervalElem.setAttribute(DAYS_ATTR_NCNAME,
                                    lInterval.getDays());
                        }
                        if (!Util.isEmpty(lInterval.getHours())) {
                            intervalElem.setAttribute(HOURS_ATTR_NCNAME,
                                    lInterval.getHours());
                        }
                        if (!Util.isEmpty(lInterval.getMinutes())) {
                            intervalElem.setAttribute(MINUTES_ATTR_NCNAME,
                                    lInterval.getMinutes());
                        }
                        if (!Util.isEmpty(lInterval.getSeconds())) {
                            intervalElem.setAttribute(SECONDS_ATTR_NCNAME,
                                    lInterval.getSeconds());
                        }
                        if (!Util.isEmpty(lInterval.getMillisecs())) {
                            intervalElem.setAttribute(MILLISECS_ATTR_NCNAME,
                                    lInterval.getMillisecs());
                        }
                    }
                }
            }
            
            if (trigger.getMessage() != null) {
                Trigger.Message lMessage = trigger.getMessage();
                Element messageElem = domDoc.createElementNS(
                        SCHEDULER_SCHEMA_NAMESPACE, MESSAGE_ELEM_NCNAME);
                triggerElem.appendChild(messageElem);
                
                if (!Util.isEmpty(lMessage.getDateFormat())) {
                    messageElem.setAttribute(DATEFORMAT_ATTR_NCNAME,
                            lMessage.getDateFormat());
                }
                if (!Util.isEmpty(lMessage.getPayload())) {
                    Text textNode =
                            domDoc.createTextNode(lMessage.getPayload());
                    messageElem.appendChild(textNode);
                }
            }
        }
        
        Source source = new DOMSource(domDoc);
        result = XmlUtil.print(source);
        
        return result;
    }
}
