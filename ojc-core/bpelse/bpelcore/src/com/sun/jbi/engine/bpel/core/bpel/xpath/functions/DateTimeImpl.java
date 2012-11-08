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
 * @(#)DateTimeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.util.GregorianCalendar;
import java.util.TimeZone;

public class DateTimeImpl {

    /**
     * @return String Current Date Time for the default Locale
     */
    public String getXSDCurrentDateTime() {
        GregorianCalendar cal = new GregorianCalendar(TimeZone.getDefault());        
        return DateParserImpl.getLocalDateTime(cal);
    }

    
    /**
     * @return String Current Date for the default Locale
     */
    public String getXSDCurrentDate() {
        GregorianCalendar cal = new GregorianCalendar(TimeZone.getDefault());        
        return DateParserImpl.getLocalDate(cal);
    }    
    
    /**
     * @return String Current Time for the default Locale
     */
    public String getXSDCurrentTime() {
        GregorianCalendar cal = new GregorianCalendar(TimeZone.getDefault());        
        return DateParserImpl.getLocalTime(cal);
    }
}
