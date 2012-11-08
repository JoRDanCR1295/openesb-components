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
 * @(#)EventHandlersOnAlarm.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.bpel.model.impl.RepeatEveryImpl;

/**
 * Describes the &lt;onAlarm&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface EventHandlersOnAlarm extends Activity, SingleActivityHolder, ForOrUntilReference {
    
    /** Tag for this element. */
    public static final String TAG = "onAlarm";
    
    /** "repeatEvery" type */
    public static final String TYPE_REPEAT_EVERY = "repeatEvery";
    
    
    /** Describes the attributes of this element.
     */
    public interface ATTR {
       
    }
    
    /** Getter for the for attribute.
     * @return  Value of the for attribute.
     */
    String getRepeatEvery();
    
    /** Setter for the for attribute.
     * @param   f   Value of the for attribute.
     */
    void setRepeatEvery(String f);
    
    
    /**
     *  set the RepeatEvery child element for this EventHandlersOnAlarm.
     * @param f for child element.
     */
    void setBPELRepeatEvery(RepeatEvery f);
    /**
     * get RepeatEvery child element of this EventHandlersOnAlarm.
     * @return For
     */
    RepeatEvery RepeatEvery() ;
}
