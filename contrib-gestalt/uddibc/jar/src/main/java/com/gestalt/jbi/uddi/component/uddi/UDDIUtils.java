/**
 *   uddi-binding-component - UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.uddi.component.uddi;

import org.uddi4j.UDDIException;

import org.uddi4j.client.UDDIProxy;

import org.uddi4j.datatype.Name;
import org.uddi4j.datatype.binding.BindingTemplate;
import org.uddi4j.datatype.binding.BindingTemplates;
import org.uddi4j.datatype.service.BusinessService;

import org.uddi4j.response.BusinessInfo;
import org.uddi4j.response.BusinessInfos;
import org.uddi4j.response.BusinessList;
import org.uddi4j.response.ServiceDetail;
import org.uddi4j.response.ServiceInfo;
import org.uddi4j.response.ServiceInfos;
import org.uddi4j.response.ServiceList;

import org.uddi4j.transport.TransportException;

import org.uddi4j.util.FindQualifier;
import org.uddi4j.util.FindQualifiers;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;


public class UDDIUtils {
    /**
     * Get all the business keys for a UDDI server
     *
     * @param businessName The buisname to search for
     * @param uddi The UDDIProxy
     * @return list of business keys (or null if none found)
     * @throws UDDIException
     * @throws TransportException
     */
    @SuppressWarnings("unchecked")
    public List<String> getBusinessKeys(String businessName, UDDIProxy uddi)
        throws UDDIException, TransportException {
        Vector<String> businessKeys = new Vector<String>();

        Vector<Name> businessNames = new Vector<Name>();
        businessNames.add(new Name(businessName));

        FindQualifiers fqs = new FindQualifiers();
        fqs.add(new FindQualifier(FindQualifier.caseSensitiveMatch));

        BusinessList businessList = uddi.find_business(businessNames, null,
                null, null, null, fqs, 0);
        BusinessInfos businessInfos = businessList.getBusinessInfos();
        Vector<BusinessInfo> businessInfoVector = (Vector<BusinessInfo>) businessInfos.getBusinessInfoVector();

        for (BusinessInfo businessInfo : businessInfoVector) {
            businessKeys.add(businessInfo.getBusinessKey());
        }

        return businessKeys;
    }

    /**
     * Perform a search against the UDDI Server
     *
     * @param serviceName The service name to search for
     * @return If matches were found it returns a <code>List</code> containing
     *         the <code>String</code>s of the URIs. If not matches were
     *         found it returns Collections.EMPTY_LIST.
     */
    @SuppressWarnings("unchecked")
    public List<String> lookupService(String serviceName, String businessName,
        UDDIProxy uddi) throws Exception {
        if (uddi == null) {
            throw new Exception("UDDIProxy can not be null");
        }

        List<String> retVal = new ArrayList<String>();

        FindQualifiers findQualifiers = new FindQualifiers();
        findQualifiers.add(new FindQualifier(FindQualifier.exactNameMatch));
        findQualifiers.add(new FindQualifier(FindQualifier.caseSensitiveMatch));

        Vector<Name> serviceNames = new Vector<Name>();
        serviceNames.addElement(new Name(serviceName));

        for (String businessKey : getBusinessKeys(businessName, uddi)) {
            ServiceList serviceList = uddi.find_service(businessKey,
                    serviceNames, null, null, findQualifiers, 0);
            ServiceInfos serviceInfos = serviceList.getServiceInfos();
            Vector<ServiceInfo> serviceInfoVector = serviceInfos.getServiceInfoVector();

            for (ServiceInfo serviceInfo : serviceInfoVector) {
                ServiceDetail serviceDetail = uddi.get_serviceDetail(serviceInfo.getServiceKey());
                Vector<BusinessService> businessServices = serviceDetail.getBusinessServiceVector();

                for (BusinessService businessService : businessServices) {
                    BindingTemplates bindingTemplates = businessService.getBindingTemplates();

                    if (bindingTemplates != null) {
                        Vector<BindingTemplate> bindings = (Vector<BindingTemplate>) bindingTemplates.getBindingTemplateVector();

                        for (BindingTemplate bindingTemplate : bindings) {
                            retVal.add(bindingTemplate.getAccessPoint().getText());
                        }
                    }
                }
            }
        }

        return retVal;
    }
}
