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
 * @(#)BarChartDataBase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.common;

import com.sun.jbi.cam.common.resources.Messages;
import java.io.Serializable;

/**
 *
 * @author graj
 */
public class BarChartDataBase implements Serializable {
    
    private Integer value;
    private String category;
    private String name;
    
    /** Creates a new instance of BarChartData */
    public BarChartDataBase() {
    }

    /** Creates a new instance of BarChartData */
    public BarChartDataBase(int xValue, String type, String yValue) {
        this.value = (new Integer(xValue)).intValue();
        this.category = type;
        this.name = yValue;
    }

    /** Creates a new instance of BarChartData */
    public BarChartDataBase(String xValue, String type, String yValue) {
        this.value = (new Integer(xValue)).intValue();
        this.category = type;
        this.name = yValue;
    }
    
    public double getValue() {
        return value;
    }

    public void setValue(int xValue) {
        this.value = (new Integer(xValue)).intValue();
    }
    
    public void setValue(String xValue) {
        this.value = (new Integer(xValue)).intValue();
    }    

    public String getCategory() {
        return category;
    }

    public void setCategory(String type) {
        this.category = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String yValue) {
        this.name = yValue;
    }
    
}
