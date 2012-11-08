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
 * @(#)PolicyGroupCollection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.policygroup;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * @author graj
 *
 */
public class PolicyGroupCollection implements Serializable {
    private static final long serialVersionUID = 1L;
	File baseLocation;
	Map<String /*policyGroupName*/, String /*baseLocation*/> policyGroupNameToFolderMap = new HashMap<String, String>();
	Map<String /*policyGroupName*/, PolicyGroup> nameToPolicyGroupMap = new HashMap<String, PolicyGroup>();
	List<PolicyGroup> policyGroupList = new ArrayList<PolicyGroup>();

	public void addPolicyGroup(PolicyGroup group) {
            PolicyGroup oldPolicyGroup = this.nameToPolicyGroupMap.get(group.getName());
            if(oldPolicyGroup != null) {
		this.policyGroupList.remove(oldPolicyGroup);
            }
            this.policyGroupList.add(group);
            this.policyGroupNameToFolderMap.put(group.getName(), group.getBaseLocation().getAbsolutePath());
            this.nameToPolicyGroupMap.put(group.getName(), group);
	}
	
	public void removePolicyGroup(String name) {
		PolicyGroup group = this.nameToPolicyGroupMap.get(name);
		if(group != null) {
			this.policyGroupList.remove(group);
			this.nameToPolicyGroupMap.remove(name);
			this.policyGroupNameToFolderMap.remove(name);
		}
	}
	

	/**
	 * 
	 */
	public PolicyGroupCollection() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the baseLocation
	 */
	public File getBaseLocation() {
		return baseLocation;
	}

	/**
	 * @param baseLocation the baseLocation to set
	 */
	public void setBaseLocation(File baseLocation) {
		this.baseLocation = baseLocation;
	}

	/**
	 * @return the nameToPolicyGroupMap
	 */
	public Map<String, PolicyGroup> getNameToPolicyGroupMap() {
		return nameToPolicyGroupMap;
	}

	/**
	 * @param nameToPolicyGroupMap the nameToPolicyGroupMap to set
	 */
	public void setNameToPolicyGroupMap(
			Map<String, PolicyGroup> nameToPolicyGroupMap) {
		this.nameToPolicyGroupMap = nameToPolicyGroupMap;
	}

	/**
	 * @return the policyGroupList
	 */
	public List<PolicyGroup> getPolicyGroupList() {
		return policyGroupList;
	}

	/**
	 * @param policyGroupList the policyGroupList to set
	 */
	public void setPolicyGroupList(List<PolicyGroup> policyGroupList) {
		this.policyGroupList = policyGroupList;
	}

	/**
	 * @return the policyGroupNameToFolderMap
	 */
	public Map<String, String> getPolicyGroupNameToFolderMap() {
		return policyGroupNameToFolderMap;
	}
        
        public PolicyGroup findPolicyGroup(String policyGroupName) {
            return this.nameToPolicyGroupMap.get(policyGroupName);
        }

	/**
	 * @param policyGroupNameToFolderMap the policyGroupNameToFolderMap to set
	 */
	public void setPolicyGroupNameToFolderMap(
			Map<String, String> policyGroupNameToFolderMap) {
		this.policyGroupNameToFolderMap = policyGroupNameToFolderMap;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
