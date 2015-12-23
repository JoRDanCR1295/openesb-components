/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor;

import java.util.ArrayList;
import bpelmonitor.model.ProcessInstance;

/**
 *
 * @author mbhasin
 */
public class InstancesList {

    private ArrayList instanceList;

    public InstancesList() {
    }

    /**
     * @return the instanceList
     */
    public ArrayList getInstanceList() {
        instanceList = new ArrayList();
        ProcessInstance instance = new ProcessInstance();
        instance.setId("ID1");
        instance.setBusinessProcessId("BP 1");
        instance.setStartTimestamp("04:20:2009 8:00");
        instance.setCompletedTimestamp("04:20:2009 9:00");
        instance.setStatus("Completed");
        instanceList.add(instance);

        return instanceList;
    }
}
