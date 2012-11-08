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
 * @(#)Activity.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.util.Collection;



/**
 * Describes what a activity-type element is.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Activity extends BPELElement, NamedElement {
    
    /** Tag for a choice of activities */
    public static final String TAG = "activity";
    
    /** Describes standard attributes for activities
     */
    public interface ATTR extends NamedElement.ATTR {
        /** "joinCondition" attribute token */
        public static final String JOIN_CONDITION = "joinCondition";
        
        /** "suppressJoinFailure" attribute token */
        public static final String SUPPRESS_JOIN_FAILURE =
            "suppressJoinFailure";
        
        /** "generateEvent" attribute token */
    	public static final String GENERATE_EVENTS = "generateEvents";
    }
    
    /** Ordinal position for joinCondition attribute. */
    public static final int JOIN_CONDITION = NAME + 1;
    
    /** Ordinal position for suppressJoinFailure attribute. */
    public static final int SUPPRESS_JOIN_FAILURE = JOIN_CONDITION + 1;
    
    /** Ordinal position for generateEvent attribute. */
    public static final int GENERATE_EVENTS = SUPPRESS_JOIN_FAILURE + 1;
    
    /** Number of standard attributes. */
    public static final int NUM_STANDARD_ATTRS = GENERATE_EVENTS + 1;
    
    /** Describes the tags for the different types of activities.
     */
    public interface Tags {
        
        /** "receive" element tag */
        public static final String RECEIVE = "receive";
        
        /** "reply" element tag */
        public static final String REPLY = "reply";
        
        /** "invoke" element tag */
        public static final String INVOKE = "invoke";
        
        /** "assign" element tag */
        public static final String ASSIGN = "assign";
        
        /** "throw" element tag */
        public static final String THROW = "throw";
        
        /** "exit" element tag */
        public static final String TERMINATE = "exit";
        
        /** "wait" element tag */
        public static final String WAIT = "wait";
        
        /** "empty" element tag */
        public static final String EMPTY = "empty";
        
        /** "sequence" element tag */
        public static final String SEQUENCE = "sequence";
        
        /** "switch" element tag */
        public static final String SWITCH = "switch";
        
        /** "while" element tag */
        public static final String WHILE = "while";
        
        /** "if" element tag */
        public static final String IF = "if";
        
        /** "pick" element tag */
        public static final String PICK = "pick";
        
        /** "flow" element tag */
        public static final String FLOW = "flow";
        
        /** "scope" element tag */
        public static final String SCOPE = "scope";
        
        /** "compensate" element tag */
        public static final String COMPENSATE = "compensate";
        
        /** "compensateScope" element tag */
        public static final String COMPENSATESCOPE = "compensateScope";

    }
    
    /** List of activity tags */
    public static final String[] ACTIVITY_TAGS = {
        Tags.RECEIVE,
        Tags.REPLY,
        Tags.INVOKE,
        Tags.ASSIGN,
        Tags.THROW,
        Tags.TERMINATE,
        Tags.WAIT,
        Tags.EMPTY,
        Tags.SEQUENCE,
        Tags.SWITCH,
        Tags.IF,
        Tags.WHILE,
        Tags.PICK,
        Tags.FLOW,
        Tags.SCOPE,
        Tags.COMPENSATE,
        Tags.COMPENSATESCOPE
    };
    
    
    /** Add a new property target to the list.
     * @param   t   The new target.
     */
    void addTarget(Target t);
    
    
    /** Removes a target element from the list.
     * @param   t   Target element to remove.
     * @return  <tt>true</tt> if target element successfully removed.
     */
    boolean removeTarget(Target t);
    
    /** Number of target elements in this activity element.
     * @return  Number of target elements.
     */
    int getTargetSize();
    
    /**
     * get a collection of <target> elements
     * @return Collection 
     */
    Collection getTargets();
    
    /**
     * get  <target> element which has given linkName attribute
     * @return Target 
     */
    Target getTarget(String linkName);
    
    /** Add a new property source to the list.
     * @param   s   The new source.
     */
    void addSource(Source s);
       
    
    /** Removes a source element from the list.
     * @param   s   Source element to remove.
     * @return  <tt>true</tt> if source element successfully removed.
     */
    boolean removeSource(Source s);
    
    /** Number of source elements in this activity element.
     * @return  Number of source elements.
     */
    int getSourceSize();
    
    /**
     * get a collection of <source> elements
     * @return Collection 
     */
    Collection getSources();
    
    /**
     * get  <source> element which has given linkName attribute
     * @return Source 
     */
    Source getSource(String linkName);
    
    /** Getter for property generateEvents.
     * @return Value of property generateEvents.
     *
     */
    String getGenerateEvents();
    
    /** Setter for property generateEvents.
     * @param generateEvents New value of property generateEvents.
     *
     */
    void setGenerateEvents(String generateEvents);
    
    
    /** Getter for property joinCondition.
     * @return Value of property joinCondition.
     *
     */
    String getJoinCondition();
    
    /** Setter for property joinCondition.
     * @param joinCondition New value of property joinCondition.
     *
     */
    void setJoinCondition(String joinCondition);
    
    /** Getter for property suppressJoinFailure.
     * @return Value of property suppressJoinFailure.
     *
     */
    String getSuppressJoinFailure();
    
    /** Setter for property suppressJoinFailure.
     * @param suppressJoinFailure New value of property suppressJoinFailure.
     *
     */
    void setSuppressJoinFailure(String suppressJoinFailure);
}
