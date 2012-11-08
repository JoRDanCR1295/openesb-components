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
 * @(#)IepFunctions.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.derby;

import java.sql.Date;
import java.sql.Timestamp;

/**
 *
 * @author Bing Lu
 */
public class IEPFunctions {
    /**
     * @return t1 - t2 in milliseconds
     */
    public static long diffInMilliseconds(Timestamp t1, Timestamp t2) {
        return t1.getTime() - t2.getTime();
    }
    
    private static final long mMillisecondsPerDay = 24 * 60 * 60 * 1000;
    public static long diffInDays(Date d1, Date d2) {
        long t1 = d1.getTime()/mMillisecondsPerDay;
        long t2 = d2.getTime()/mMillisecondsPerDay;
        
        return (t1 - t2);
    }
    
    private static final long mStartMilliSecond = System.currentTimeMillis();
    private static final long mStartNanoSecond = System.nanoTime();
    
    public static Timestamp currentTimestampWithNanoseconds() {
        //This method relies on System.nanoTime() which is not accurate when used on
        //Solaris, so temporarily using the SQL CURRENT_TIMESTAMP */
    	
        long elapsedNanoSecond = System.nanoTime() - mStartNanoSecond;
        long curTime_Second = (mStartMilliSecond + elapsedNanoSecond/1000000)/1000;
        int curTime_NanoSecond = (((int)(mStartMilliSecond%1000))*1000000 + (int)(elapsedNanoSecond%1000000000))%1000000000;
                
        Timestamp t = new Timestamp(curTime_Second * 1000);
        t.setNanos(curTime_NanoSecond);
        return t;
    }
    
}
