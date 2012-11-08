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
 * @(#)AbstractServicesBuilder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.services;

import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;
import org.xml.sax.InputSource;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.model.Services;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractServicesBuilder implements ServicesBuilder {
    private List<EndpointInfo> mEndpts;
    private int mNumProvides = 0;
    private Map<String, Definition> mWsdlCache;
    
    protected AbstractServicesBuilder() {
        mEndpts = new ArrayList<EndpointInfo>();
        mWsdlCache = new HashMap<String, Definition>();
    }
    
    /** @see com.sun.jbi.component.toolkit.project.services.ServicesBuilder#generateServices(java.lang.String, java.lang.String) */
    public abstract Services generateServices(String unitName, String artifactRoot);

    protected void addService(EndpointInfo info) {
        if (isValidService(info)) { // TODO check for duplicate service, throw?
            if (info.isProvides()) {
                mEndpts.add(mNumProvides, info);
                ++mNumProvides;
            }
            else {
                mEndpts.add(info);
            }
        }
    }

    protected Services createServices() {
        int size = mEndpts.size();
        EndpointInfo[] prov = new EndpointInfo[mNumProvides],
                       cons = new EndpointInfo[size - mNumProvides];
        for (int i = 0; i < size; i++) {
            EndpointInfo info = mEndpts.get(i);
            if (info.isProvides()) {
                prov[i] = info;
            }
            else {
                cons[i - mNumProvides] = info;
            }
        }
        return new Services(prov, cons);
    }
    
    protected Map<String, Definition> getWsdls() {
        return mWsdlCache;
    }
    
    protected void loadWsdlCache(File path) {
        if (path != null) {
            if (path.isDirectory()) {
                File[] wsdls = path.listFiles(new FilenameFilter() {
                    public boolean accept(File dir, String name) {
                        return (!Util.isEmpty(name) && name.endsWith(".wsdl"));
                    }
                });
                for (File f : wsdls) {
                    readWsdl(f);
                }
            }
            else if (!Util.isEmpty(path.getName()) && path.getName().endsWith(".wsdl")) {
                readWsdl(path);
            }
        }
    }

    protected String readWsdl(File f) {
        if (f != null && f.exists()) {
            try {
                WSDLReader reader = WSDL4JExt.newWSDLReader(null);
                Definition def = 
                        reader.readWSDL(f.toURI().toString(), 
                                        new InputSource(new FileReader(f)));
                if (def != null) {
                    getWsdls().put(def.getTargetNamespace(), def);
                }
                return def.getTargetNamespace();
            }
            catch (Exception e) {
                throw new ProjectException(
                        "Failed to load wsdl at "+ f.getAbsolutePath(), e);
            }
        }
        
        return null;
    }
    
    protected void removeService(EndpointInfo info) {
        if (isValidService(info) && mEndpts.contains(info)) {
            mEndpts.remove(info);
        }
    }

    protected boolean isValidService(EndpointInfo info) {
        return (info != null && info.getServiceName() != null &&
                info.getInterfaceName() != null && !Util.isEmpty(info.getEndpointName()));
    }
}
