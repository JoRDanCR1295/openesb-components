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
 * @(#)Target.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.model;

/**
 * Models a service unit target, as defined in a service assembly descriptor.
 * @author Kevan Simpson
 */
public class Target {
    private String mArtifactsZip, mComponentName;

    /**
     * Constructs a <code>Target</code>.
     * @param artifactsZip The deployment artifact.
     * @param componentName The component to which unit is deployed.
     */
    public Target(String artifactsZip, String componentName) {
        mArtifactsZip = artifactsZip;
        mComponentName = componentName;
    }
    
    /**
     * Returns the artifacts zip.
     * @return the artifacts zip.
     */
    public String getArtifactsZip() {
        return mArtifactsZip;
    }

    /**
     * Returns the component name.
     * @return the component name.
     */
    public String getComponentName() {
        return mComponentName;
    }
}
