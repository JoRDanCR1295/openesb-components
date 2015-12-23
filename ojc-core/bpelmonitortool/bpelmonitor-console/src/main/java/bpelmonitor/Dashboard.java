/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor;

import bpelmonitor.jbiruntime.BPELSERuntime;
import bpelmonitor.model.DashboardEntry;
import com.icesoft.faces.component.ext.RowSelectorEvent;
import java.util.ArrayList;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.servlet.http.HttpSession;
import javax.xml.namespace.QName;

/**
 *
 * @author mbhasin
 */
public class Dashboard {

    private ArrayList entries;

    public Dashboard() {
    }

    public ArrayList getEntries() {
        entries = new ArrayList();

//        ArrayList<DashboardEntry> dashboardEntries = getDashboardData();
//        DashboardEntry entryTO = null;
//        DashboardEntry entry = null;
//
//        if (dashboardEntries != null && dashboardEntries.size() > 0) {
//            for (Iterator<DashboardEntry> iter = dashboardEntries.iterator(); iter.hasNext();) {
//                entryTO = iter.next();
//                entry = new DashboardEntry();
//                entry.setApplicaitonName(entryTO.getApplicaitonName());
//                entry.setBusinessProcessName((QName.valueOf(entryTO.getBusinessProcessName())).getLocalPart() + ".bpel");
//                entry.setLastInstanceProcessedTime(entryTO.getLastInstanceProcessedTime());
//                entry.setLiveInstnces(entryTO.getLiveInstnces());
//                entry.setSuspendedInstances(entryTO.getSuspendedInstances());
//                entry.setFaultedInstances(entryTO.getFaultedInstances());
//                entry.setTerminatedInstances(entryTO.getTerminatedInstances());
//                entry.setKpi(entryTO.getKpi());
//                entries.add(entry);
//            }
//        }

        return entries;
    }

    ArrayList<DashboardEntry> getEntries(ArrayList<DashboardEntry> entries1) {

        ArrayList<DashboardEntry> dashboardEntries = new BPELSERuntime().getLiveInstances(entries1);
        DashboardEntry entryTO = null;
        DashboardEntry entry = null;

        ArrayList<DashboardEntry> dashboardData = new ArrayList();
        if (dashboardEntries != null && dashboardEntries.size() > 0) {
            for (Iterator<DashboardEntry> iter = dashboardEntries.iterator(); iter.hasNext();) {
                entryTO = iter.next();
                entry = new DashboardEntry();
                entry.setApplicaitonName(entryTO.getApplicaitonName());
                entry.setBusinessProcessName((QName.valueOf(entryTO.getBusinessProcessName())).getLocalPart() + ".bpel");
                entry.setLastInstanceProcessedTime(entryTO.getLastInstanceProcessedTime());
                entry.setLiveInstnces(entryTO.getLiveInstnces());
                entry.setSuspendedInstances(entryTO.getSuspendedInstances());
                entry.setFaultedInstances(entryTO.getFaultedInstances());
                entry.setTerminatedInstances(entryTO.getTerminatedInstances());
                entry.setKpi(entryTO.getKpi());
                dashboardData.add(entry);
            }
        }

        return dashboardData;
    }

    private ArrayList<DashboardEntry> getDashboardData() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        ArrayList<DashboardEntry> dashboardEntries = (ArrayList<DashboardEntry>) session.getAttribute(BPList.DASHBOARD_ENTRIES);
        dashboardEntries = new BPELSERuntime().getLiveInstances(dashboardEntries);
        return dashboardEntries;
    }

    /**
     * SelectionListener bound to the ice:rowSelector component.  Called
     * when a row is selected in the UI.
     *
     * @param event from the ice:rowSelector component
     */
    public void rowSelectionListener(RowSelectorEvent event) {
        UIComponent comp = event.getComponent();
        DashboardEntry entry = null;
        for (int i = 0, max = entries.size(); i < max; i++) {
            entry = (DashboardEntry) entries.get(i);
            if (entry.isSelected()) {
                FacesContext facesContext = FacesContext.getCurrentInstance();
                HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
                ((SessionBean1)session).setSelectedProcess(entry);
            }
        }
    }

    public void rowClickListener(ActionEvent event) {
        UIComponent comp = event.getComponent();

    }
}
