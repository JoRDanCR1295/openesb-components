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
 * @(#)OtdLocation.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

/**
 * The OtdLocation information provides a cookie interface for location
 * information about data inside an OTD.  This may refer to a combination of
 * input data location (e.g. byte offset or line/column indication), node
 * instance location in its tree (symbolic or numeric path expression),
 * original metadata references, and OTD context information (e.g. the OTD
 * instance name).
 *
 * This interface is currently [2003-09-05] under development.
 *
 * @author Michael Libourel
 * @version
 */
public interface OtdLocation {
    /**
     * Converts the information to a printable string, suitable for inclusion
     * in a basic textual error message.
     *
     * @return a text string
     */
    String text();

    /**
     * Gives the enclosing location context, if any.
     * For embedded parsing, this should return the information on the
     * node containg the embedded section.
     *
     * @return the location, or null if none
     */
    OtdLocation from();

    /**
     * Gets a property by name.
     * This is a generic extension mechanism.
     *
     * @param name  the property name
     * @return the value, or null if not known
     */
    //-not until 5.0.4: Object getProperty (String name);
}
