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
 * @(#)VehicleInformationService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.fs.vehicle;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author gpatil
 */
@Stateless
@WebService(serviceName = "VehicleInformationServerService", portName = "VehicleInformationServerPort", endpointInterface = "com.fs.vehicle.VehicleInformationServer", targetNamespace = "http://vehicle.fs.com/", wsdlLocation = "META-INF/wsdl/VehicleInformationService/VehicleInformationService.wsdl")
public class VehicleInformationService implements com.fs.vehicle.VehicleInformationServer {
    
    /** Creates a new instance of VehicleInformationService */
    public VehicleInformationService() {
    }

    public String getVehicleData(String vehicleIdentificationNumber, String make, String model, String year) {
        System.out.println("VehicleInformationServer::getVehicleData start");
        
        String result = null;
        final String SEPARATOR = "|";
        //Sample code - to be replaced with functional code.
        String score="305", category="a";
        result = category+SEPARATOR+score;
        System.out.println("VehicleInformationServer::getVehicleData end. result: "+result);
        return result;

    }
    
}
