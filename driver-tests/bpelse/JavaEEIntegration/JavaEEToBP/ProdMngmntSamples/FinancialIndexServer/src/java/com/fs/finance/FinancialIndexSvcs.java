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
 * @(#)FinancialIndexSvcs.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.fs.finance;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author gpatil
 */
@Stateless
@WebService(serviceName = "FinancialIndexService", portName = "FinancialIndexPort", endpointInterface = "com.fs.finance.FinancialIndex", targetNamespace = "http://finance.fs.com/", wsdlLocation = "META-INF/wsdl/FinancialIndexSvcs/FinancialIndexService.wsdl")
public class FinancialIndexSvcs implements com.fs.finance.FinancialIndex {
    
    /** Creates a new instance of FinancialIndexSvcs */
    public FinancialIndexSvcs() {
    }

    public int getFinancialIndex(String category) {
        System.out.println("FinancialIndex::getFinancialIndex start");
        int index = 205;
        System.out.println("FinancialIndex::getFinancialIndex end. result: "+index);
        return index;
    }
    
}
