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
 * @(#)PerformanceEvaluatorBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.fs.customer;

import javax.ejb.Stateless;

/**
 *
 * @author pbalashanmugam
 */
@Stateless
public class PerformanceEvaluatorBean implements PerformanceEvaluatorRemote, PerformanceEvaluatorLocal {
    
    /** Creates a new instance of PerformanceEvaluatorBean */
    public PerformanceEvaluatorBean() {
    }

    public String getCustomerReport(String socialSecurityNumber, String customerIdentification, String accountID, String firstName, String lastName) {
        System.out.println("PerformanceEvaluatorBean::getCustomerReport start");
        String result = null;
        final String SEPARATOR = "|";
        String risk = "low", score = "1705";
        result = risk+SEPARATOR+score;
        System.out.println("PerformanceEvaluatorBean::getCustomerReport end. result: "+result);
        return result;
    }
    
    
    
}
