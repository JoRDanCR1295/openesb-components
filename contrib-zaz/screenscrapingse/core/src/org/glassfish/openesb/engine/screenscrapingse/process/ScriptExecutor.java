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
 * @(#)ScriptExecutor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.engine.screenscrapingse.process;

import javax.xml.transform.Source;

/**
 *
 * @author Prashanth B.R
 */
public interface ScriptExecutor {

    /**
     * This is the core script processing method, which will accept a Dom source file
     * contain script data\meta data required for script execution. The result is returned as
     * another source content.
     *
     * @param aInputSource DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Source processScript(Source aInputSource);
} //interface ScriptExecutor ends.
