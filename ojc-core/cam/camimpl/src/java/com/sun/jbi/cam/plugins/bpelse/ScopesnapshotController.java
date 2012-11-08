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
 * @(#)ScopesnapshotController.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.plugins.bpelse.datamodel.Scopesnapshot;
import com.sun.jbi.cam.plugins.bpelse.datamodel.ScopesnapshotPK;
import com.sun.jbi.cam.plugins.bpelse.datamodel.State;
import com.sun.jbi.cam.plugins.bpelse.datamodel.Variable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.annotation.Resource;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.model.DataModel;
import javax.faces.model.ListDataModel;
import javax.faces.model.SelectItem;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.persistence.Query;
import javax.transaction.UserTransaction;

/**
 *
 * @author nnahata
 */
@SuppressWarnings("unchecked")
public class ScopesnapshotController {
    
    /** Creates a new instance of ScopesnapshotController */
    public ScopesnapshotController() {
    }

    private Scopesnapshot scopesnapshot;

    private DataModel model;

    @Resource
    private UserTransaction utx;

    //@PersistenceUnit(unitName = "cambpelsePU")
    private EntityManagerFactory emf;

    private EntityManager getEntityManager() {
        return emf.createEntityManager();
    }

    private int batchSize = 20;

    private int firstItem = 0;

    public Scopesnapshot getScopesnapshot() {
        return scopesnapshot;
    }

    public void setScopesnapshot(Scopesnapshot scopesnapshot) {
        this.scopesnapshot = scopesnapshot;
    }

    public DataModel getDetailScopesnapshots() {
        return model;
    }

    public void setDetailScopesnapshots(Collection<Scopesnapshot> m) {
        model = new ListDataModel(new ArrayList(m));
    }

    public String createSetup() {
        this.scopesnapshot = new Scopesnapshot();
        return "scopesnapshot_create";
    }

    public String create() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            em.persist(scopesnapshot);
            utx.commit();
            addSuccessMessage("Scopesnapshot was successfully created.");
        } catch (Exception ex) {
            try {
                addErrorMessage(ex.getLocalizedMessage());
                utx.rollback();
            } catch (Exception e) {
                addErrorMessage(e.getLocalizedMessage());
            }
        } finally {
            em.close();
        }
        return "scopesnapshot_list";
    }

    public String detailSetup() {
        setScopesnapshotFromRequestParam();
        return "scopesnapshot_detail";
    }

    public String editSetup() {
        setScopesnapshotFromRequestParam();
        return "scopesnapshot_edit";
    }

    public String edit() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            scopesnapshot = em.merge(scopesnapshot);
            utx.commit();
            addSuccessMessage("Scopesnapshot was successfully updated.");
        } catch (Exception ex) {
            try {
                addErrorMessage(ex.getLocalizedMessage());
                utx.rollback();
            } catch (Exception e) {
                addErrorMessage(e.getLocalizedMessage());
            }
        } finally {
            em.close();
        }
        return "scopesnapshot_list";
    }

    public String destroy() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            Scopesnapshot scopesnapshot = getScopesnapshotFromRequestParam();
            scopesnapshot = em.merge(scopesnapshot);
            em.remove(scopesnapshot);
            utx.commit();
            addSuccessMessage("Scopesnapshot was successfully deleted.");
        } catch (Exception ex) {
            try {
                addErrorMessage(ex.getLocalizedMessage());
                utx.rollback();
            } catch (Exception e) {
                addErrorMessage(e.getLocalizedMessage());
            }
        } finally {
            em.close();
        }
        return "scopesnapshot_list";
    }

    public Scopesnapshot getScopesnapshotFromRequestParam() {
        EntityManager em = getEntityManager();
        try{
            Scopesnapshot o = (Scopesnapshot) model.getRowData();
            o = em.merge(o);
            return o;
        } finally {
            em.close();
        }
    }

    public void setScopesnapshotFromRequestParam() {
        Scopesnapshot scopesnapshot = getScopesnapshotFromRequestParam();
        setScopesnapshot(scopesnapshot);
    }

    public DataModel getScopesnapshots() {
        EntityManager em = getEntityManager();
        try{
            Query q = em.createQuery("select object(o) from Scopesnapshot as o");
            q.setMaxResults(batchSize);
            q.setFirstResult(firstItem);
            model = new ListDataModel(q.getResultList());
            return model;
        } finally {
            em.close();
        }
    }

    public static void addErrorMessage(String msg) {
        FacesMessage facesMsg = new FacesMessage(FacesMessage.SEVERITY_ERROR, msg, msg);
        FacesContext fc = FacesContext.getCurrentInstance();
        fc.addMessage(null, facesMsg);
    }

    public static void addSuccessMessage(String msg) {
        FacesMessage facesMsg = new FacesMessage(FacesMessage.SEVERITY_INFO, msg, msg);
        FacesContext fc = FacesContext.getCurrentInstance();
        fc.addMessage("successInfo", facesMsg);
    }

    public Scopesnapshot findScopesnapshot(ScopesnapshotPK id) {
        EntityManager em = getEntityManager();
        try{
            Scopesnapshot o = (Scopesnapshot) em.find(Scopesnapshot.class, id);
            return o;
        } finally {
            em.close();
        }
    }

    public javax.faces.model.SelectItem[] getStates() {
        EntityManager em = getEntityManager();
        try{
            List <State> l = (List <State>) em.createQuery("select o from State as o").getResultList();
            SelectItem select[] = new SelectItem[l.size()];
            int i = 0;
            for(State x : l) {
                    select[i++] = new SelectItem(x);
                }
                return select;
        } finally {
            em.close();
        }
    }

    public javax.faces.model.SelectItem[] getVariables() {
        EntityManager em = getEntityManager();
        try{
            List <Variable> l = (List <Variable>) em.createQuery("select o from Variable as o").getResultList();
            SelectItem select[] = new SelectItem[l.size()];
            int i = 0;
            for(Variable x : l) {
                    select[i++] = new SelectItem(x);
                }
                return select;
        } finally {
            em.close();
        }
    }

    public int getItemCount() {
        EntityManager em = getEntityManager();
        try{
            int count = ((Long) em.createQuery("select count(o) from Scopesnapshot as o").getSingleResult()).intValue();
            return count;
        } finally {
            em.close();
        }
    }

    public int getFirstItem() {
        return firstItem;
    }

    public int getLastItem() {
        int size = getItemCount();
        return firstItem + batchSize > size ? size : firstItem + batchSize;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public String next() {
        if (firstItem + batchSize < getItemCount()) {
            firstItem += batchSize;
        }
        return "scopesnapshot_list";
    }

    public String prev() {
        firstItem -= batchSize;
        if (firstItem < 0) {
            firstItem = 0;
        }
        return "scopesnapshot_list";
    }
    
}
