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
 * @(#)ParameterType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.model.aspects;

/**
 * @author graj
 *
 */
public enum ParameterType {
    INPUT("input"), OUTPUT("output");

    String direction;

    /** @param protocolString */
    private ParameterType(String directionString) {
        this.direction = directionString;
    }

    /** @return the direction */
    public String getDirection() {
        return direction;
    }

    /** @return the direction */
    public String getDescription() {
        switch (this) {
        case INPUT:
            return "INPUT Parameter Type";
        case OUTPUT:
            return "OUTPUT Parameter Type";
        default:
            return "Unknown";
        }
    }
}
