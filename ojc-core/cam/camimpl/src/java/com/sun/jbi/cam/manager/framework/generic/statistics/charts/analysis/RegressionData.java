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
 * @(#)RegressionData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic.statistics.charts.analysis;

import java.io.Serializable;

/**
 *
 * @author graj
 */
public class RegressionData implements Serializable {
    
    private double aValue;
    private double bValue;
    private double correlationCoefficient;
    private String regressionEquation;
    
    /** Creates a new instance of RegressionData */
    public RegressionData() {
    }
    
    /** Creates a new instance of RegressionData */
    public RegressionData(double iValue, double jValue, double coefficient, String equation) {
        this.setAValue(iValue);
        this.setBValue(jValue);
        this.setCorrelationCoefficient(coefficient);
        this.setRegressionEquation(equation);
    }

    public double getAValue() {
        return aValue;
    }

    public void setAValue(double aValue) {
        this.aValue = aValue;
    }

    public double getBValue() {
        return bValue;
    }

    public void setBValue(double bValue) {
        this.bValue = bValue;
    }

    public double getCorrelationCoefficient() {
        return correlationCoefficient;
    }

    public void setCorrelationCoefficient(double correlationCoefficient) {
        this.correlationCoefficient = correlationCoefficient;
    }

    public String getRegressionEquation() {
        return regressionEquation;
    }

    public void setRegressionEquation(String regressionEquation) {
        this.regressionEquation = regressionEquation;
    }
}
