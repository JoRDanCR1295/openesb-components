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

import org.uddi4j.datatype.binding.AccessPoint;
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

import org.uddi4j.util.CategoryBag;
import org.uddi4j.util.DiscoveryURLs;
import org.uddi4j.util.FindQualifiers;
import org.uddi4j.util.IdentifierBag;
import org.uddi4j.util.TModelBag;

import java.util.Vector;


/**
 * Created by IntelliJ IDEA.
 * User: panderson
 * Date: May 29, 2007
 * Time: 12:34:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class MockUDDIProxy extends UDDIProxy {
    public BusinessList find_business(Vector vector,
        DiscoveryURLs discoveryURLs, IdentifierBag identifierBag,
        CategoryBag categoryBag, TModelBag tModelBag,
        FindQualifiers findQualifiers, int i)
        throws UDDIException, TransportException {
        BusinessInfo businessInfo = new BusinessInfo();
        businessInfo.setBusinessKey("BusinessKey1");

        Vector<BusinessInfo> businessInfoVector = new Vector<BusinessInfo>();
        businessInfoVector.add(businessInfo);

        BusinessInfos businessInfos = new BusinessInfos();
        businessInfos.setBusinessInfoVector(businessInfoVector);

        BusinessList businessList = new BusinessList();
        businessList.setBusinessInfos(businessInfos);

        return businessList;
    }

    public ServiceList find_service(String string, Vector vector,
        CategoryBag categoryBag, TModelBag tModelBag,
        FindQualifiers findQualifiers, int i)
        throws UDDIException, TransportException {
        ServiceInfo serviceInfo = new ServiceInfo();
        serviceInfo.setBusinessKey("BusinessKey1");
        serviceInfo.setServiceKey("ServiceKey1");

        Vector<ServiceInfo> serviceInfoVector = new Vector<ServiceInfo>();
        serviceInfoVector.add(serviceInfo);

        ServiceInfos serviceInfos = new ServiceInfos();
        serviceInfos.setServiceInfoVector(serviceInfoVector);

        ServiceList servicelist = new ServiceList();
        servicelist.setServiceInfos(serviceInfos);

        return servicelist;
    }

    public ServiceDetail get_serviceDetail(String string)
        throws UDDIException, TransportException {
        AccessPoint accessPoint = new AccessPoint();
        accessPoint.setText("http://service@domain.ext");

        BindingTemplate bindingTemplate = new BindingTemplate();
        bindingTemplate.setAccessPoint(accessPoint);

        Vector<BindingTemplate> bindingTemplateVector = new Vector<BindingTemplate>();
        bindingTemplateVector.add(bindingTemplate);

        BindingTemplates bindingTemplates = new BindingTemplates();
        bindingTemplates.setBindingTemplateVector(bindingTemplateVector);

        BusinessService businessService = new BusinessService();
        businessService.setBindingTemplates(bindingTemplates);

        Vector<BusinessService> businessServiceVector = new Vector<BusinessService>();
        businessServiceVector.add(businessService);

        ServiceDetail serviceDetail = new ServiceDetail();
        serviceDetail.setBusinessServiceVector(businessServiceVector);

        return serviceDetail;
    }
}
