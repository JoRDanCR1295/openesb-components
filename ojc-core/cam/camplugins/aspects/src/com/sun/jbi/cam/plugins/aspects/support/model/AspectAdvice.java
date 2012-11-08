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
 * @(#)AspectAdvice.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

import java.io.Serializable;

/**
 * @author graj
 *
 */
public class AspectAdvice implements Serializable {
	private static final long serialVersionUID = 1L;
	
    AspectType aspectType;
    String configurationFile;
    Integer order;
    
    /**
     * 
     */
    public AspectAdvice() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param aspectType
     * @param configurationFile
     * @param order
     */
    public AspectAdvice(AspectType aspectType, String configurationFile, Integer order) {
        super();
        this.aspectType = aspectType;
        this.configurationFile = configurationFile;
        this.order = order;
    }

    /**
     * @return the aspectType
     */
    public AspectType getAspectType() {
        return aspectType;
    }

    /**
     * @param aspectType the aspectType to set
     */
    public void setAspectType(AspectType aspectType) {
        this.aspectType = aspectType;
    }

    /**
     * @return the configurationFile
     */
    public String getConfigurationFile() {
        return configurationFile;
    }

    /**
     * @param configurationFile the configurationFile to set
     */
    public void setConfigurationFile(String configurationFile) {
        this.configurationFile = configurationFile;
    }

    /**
     * @return the order
     */
    public Integer getOrder() {
        return order;
    }

    /**
     * @param order the order to set
     */
    public void setOrder(Integer order) {
        this.order = order;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
