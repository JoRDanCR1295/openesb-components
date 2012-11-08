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
 * @(#)Time.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt;


/**
 * Time interface
 *
 * @author Sun Microsystems
 *         Preferences - Java - Code Generation - Code and Comments
 */
public interface Time {
    /**
     * get hours
     *
     * @return int hours
     */
    public int getHours();

    /**
     * get minutes
     *
     * @return int minutes
     */
    public int getMinutes();

    /**
     * get seconds
     *
     * @return int seconds
     */
    public int getSeconds();

    /**
     * get milliseconds
     *
     * @return int milliseconds
     */
    public int getMilliseconds();

    /**
     * set hours
     *
     * @param hours hours
     */
    public void setHours(int hours);

    /**
     * set minutes
     *
     * @param minutes minutes
     */
    public void setMinutes(int minutes);

    /**
     * set seconds
     *
     * @param seconds seconds
     */
    public void setSeconds(int seconds);

    /**
     * set milliseconds
     *
     * @param millis milliseconds
     */
    public void setMilliseconds(int millis);
}
