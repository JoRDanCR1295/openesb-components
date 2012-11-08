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
 * @(#)ToDoEvent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.todotask;

import java.util.EventObject;
import java.util.logging.Level;

/**
 * Implements the to-do task event.
 *
 * @author Sun Microsystems
 * @version 
 */
public class ToDoEvent extends EventObject {
    
    /** Holds the category of the event. */
    private int category;
    
    /** Holds the severity of the event. */
    private int severity;
    
    /** Holds the description of the event. */
    private String description;
    
    /** Holds the recourse of the event. */
    private String recourse;
    
    /** Describes the categories of to-do events.
     */
    public interface Category {
        /** BPEL Syntax */
        public static final int BPEL_SYNTAX = 2010;
        
        /** BPEL Syntax string */
        public static final String BPEL_SYNTAX_STR = "BPEL Syntax";
        
        /** BPEL Semantics */
        public static final int BPEL_SEMANTICS = 2020;
        
        /** BPEL Semantics string */
        public static final String BPEL_SEMANTICS_STR = "BPEL Semantics";

        /** WSDL Syntax */
        public static final int WSDL_SYNTAX = 5010;
        
        /** WSDL Syntax string */
        public static final String WSDL_SYNTAX_STR = "WSDL Syntax";
        
        /** WSDL Semantics */
        public static final int WSDL_SEMANTICS = 5020;
        
        /** WSDL Semantics string */
        public static final String WSDL_SEMANTICS_STR = "WSDL Semantics";
        
    }
    
    /** Describes the Severity of to-do events.
     */
    public interface Severity {
        /** Error */
        public static final int ERROR = Level.SEVERE.intValue();
        
        /** Error string */
        public static final String ERROR_STR = "Error";
        
        /** Warning */
        public static final int WARNING = Level.WARNING.intValue();
        
        /** Warning string */
        public static final String WARNING_STR = "Warning";
        
        /** Information */
        public static final int INFO = Level.INFO.intValue();
        
        /** Information string */
        public static final String INFO_STR = "Information";
    }
    
    /** Creates a new instance of ToDoEvent.
     *  @param  source  The source of the to-do event.
     */
    public ToDoEvent(Object source) {
        super(source);
    }
    
    /** Creates a new instance of ToDoEvent.
     * @param   source      The source of the to-do event.
     * @param   category    Category for the event.
     * @param   severity    Severity for the event.
     * @param   description Description for the event.
     * @param   recourse    Recourse for the event.
     */
    public ToDoEvent(Object source, int category, int severity, String description, String recourse) {
        super(source);
        setCategory(category);
        setSeverity(severity);
        setDescription(description);
        setRecourse(recourse);
    }
    
    /** Sets the category for the event.
     * @param   category    Category for the event.
     */
    public void setCategory(int category) {
        this.category = category;
    }
    
    /** Gets the category for the event.
     * @return  Category for the event.
     */
    public int getCategory() {
        return category;
    }
    
    /** Sets the severity for the event.
     * @param   severity    Severity for the event.
     */
    public void setSeverity(int severity) {
        this.severity = severity;
    }
    
    /** Gets the severity for the event.
     * @return  Severity for the event.
     */
    public int getSeverity() {
        return severity;
    }
    
    /** Sets the description for the event.
     * @param   description     Description for the event.
     */
    public void setDescription(String description) {
        this.description = description;
    }
    
    /** Gets the description for the event.
     * @return  Description for the event.
     */
    public String getDescription() {
        return description;
    }
    
    /** Sets the recourse for the event.
     * @param   recourse    Recourse for the event.
     */
    public void setRecourse(String recourse) {
        this.recourse = recourse;
    }
    
    /** Gets the recourse for the event.
     * @return  Recourse for the event.
     */
    public String getRecourse() {
        return recourse;
    }
}
