 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import javax.management.MBeanException;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.OpenDataException;

import net.java.hulp.measure.Group;

import com.sun.jbi.eManager.provider.PerformanceMeasurement;

/**
 *
 * 
 */
public class Jbi4CorbaPerformanceMeasurement implements PerformanceMeasurement {

    public static final String JBI4CORBA_BC_SOURCE_FILTER = "it.imolinfo.jbi4corba.*";
    
    /** 
     * Retrieves the performance insturmentation measurement for the specified endpoint. 
     * @param endpoint The endpoint name qualified by service name.
     * @return An instance of TabularData which holds the performance instrumentation measurement for the specified endpoint.
     * @throws OpenDataException if there's an error related to the openmbean data.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
     public TabularData getPerformanceInstrumentationMeasurement(String endpoint) throws OpenDataException, MBeanException {
        Group gr = Group.createGroup();
        gr.addPattern(JBI4CORBA_BC_SOURCE_FILTER, endpoint, getPerfCatFilter());
        return gr.fetchData();
    }

    /** 
     * Resets the performance measurements on the endpoint.
     * @param endpoint The endpoint name qualified by service name.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public void clearPeformaceInstrumentationMeasurement (String endpoint) throws MBeanException {
        Group gr = Group.createGroup();
        gr.addPattern(JBI4CORBA_BC_SOURCE_FILTER, endpoint, getPerfCatFilter());
        gr.clearData();
    }

    private String getPerfCatFilter() {
        StringBuffer perfCatFilter = new StringBuffer();
        for (int i=0; i < Jbi4CorbaLifeCycle.JBI4CORBABC_PERF_CATEGORIES.length; i++) {
            perfCatFilter.append(Jbi4CorbaLifeCycle.JBI4CORBABC_PERF_CATEGORIES[i]);
            if (i < Jbi4CorbaLifeCycle.JBI4CORBABC_PERF_CATEGORIES.length-1) {
                perfCatFilter.append('|');
            }
        }
        return perfCatFilter.toString();
    }
}
