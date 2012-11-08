package com.sun.jbi.imsbc;

import javax.management.MBeanException;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.OpenDataException;

import net.java.hulp.measure.Group;

import com.sun.jbi.eManager.provider.PerformanceMeasurement;

/**
 * Implementation of PerformanceMeasurement
 *
 * @author Sun Microsystems
 */

public class IMSPerformanceMeasurement implements PerformanceMeasurement {

    public static final String IMS_BC_SOURCE_FILTER = ".*imsbc.*";

    /** 
     * Retrieves the performance insturmentation measurement for the specified endpoint. 
     * @param endpoint The endpoint name qualified by service name.
     * @return An instance of TabularData which holds the performance instrumentation measurement for the specified endpoint.
     * @throws OpenDataException if there's an error related to the openmbean data.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public TabularData getPerformanceInstrumentationMeasurement(String endpoint) throws OpenDataException, MBeanException {
        Group grp = Group.createGroup();
        grp.addPattern(IMS_BC_SOURCE_FILTER, endpoint, getPerfCatFilter());
        return grp.fetchData();
    }

    /** 
     * Resets the performance measurements on the endpoint.
     * @param endpoint The endpoint name qualified by service name.
     * @throws MBeanException if the endpoint does not exist for the component.
     */
    public void clearPeformaceInstrumentationMeasurement (String endpoint) throws MBeanException {
        Group grp = Group.createGroup();
        grp.addPattern(IMS_BC_SOURCE_FILTER, endpoint, getPerfCatFilter());
        grp.clearData();
    }

    private String getPerfCatFilter() {
        StringBuffer perfCatFilter = new StringBuffer();
        for (int i=0; i < IMSBindingComponent.IMS_PERF_CATEGORIES.length; i++) {
            perfCatFilter.append(IMSBindingComponent.IMS_PERF_CATEGORIES[i]);
            if (i < IMSBindingComponent.IMS_PERF_CATEGORIES.length-1) {
                perfCatFilter.append('|');
            }
        }
        return perfCatFilter.toString();
    }
}
