/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;


//import com.sun.nvs.core.devicemodules.grouping.ComboGroupingRule;
import java.util.ArrayList;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class DynamicDeviceGroup implements DeviceGroup {
    private static final DeviceGroup[] _nullChildren = new DeviceGroup[0];

    /**
     * DOCUMENT ME!
     */
    protected int groupId;

    /**
     * DOCUMENT ME!
     */
    protected String groupName;

    /**
     * DOCUMENT ME!
     */
    protected GroupingRule groupingRule;

    /**
     * DOCUMENT ME!
     */
    protected Network parentNetwork;

    /**
     * DOCUMENT ME!
     */
    protected EntityId entId;

    /**
     * DOCUMENT ME!
     */
    protected boolean _systemGroup = false;

    /**
     * Creates a new DynamicDeviceGroup object.
     *
     * @param parentNet DOCUMENT ME!
     * @param name DOCUMENT ME!
     * @param rule DOCUMENT ME!
     */
    public DynamicDeviceGroup(Network parentNet, String name, GroupingRule rule) {
        parentNetwork = parentNet;
        groupName = name;
        groupingRule = rule;
        groupId = DeviceIdGenerator.nextId();

        entId = new EntityId("/network/groups/" + name);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getId() {
        return groupId;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getName() {
        return groupName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public EntityId getEntityId() {
        return entId;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isContainer() {
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param obj DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean equals(Object obj) {
        if (obj instanceof DynamicDeviceGroup) {
            DynamicDeviceGroup dg = (DynamicDeviceGroup) obj;

            return (dg.groupId == groupId);
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isSystemGroup() {
        return _systemGroup;
    }

    /**
     * DOCUMENT ME!
     *
     * @param isSysGrp DOCUMENT ME!
     */
    public void setSystemGroup(boolean isSysGrp) {
        _systemGroup = isSysGrp;
    }

    /**
     * DOCUMENT ME!
     *
     * @param d DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isMember(Device d) {
        if (groupingRule == null) {
            return true;
        } else {
            return groupingRule.includeInGroup(d);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param g DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean containsGroup(DeviceGroup g) {
        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public DeviceGroup[] getChildGroups() {
        return _nullChildren;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getImmediateLeafMembers() {
        return getAllLeafMembers(parentNetwork);
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getAllLeafMembers() {
        return getAllLeafMembers(parentNetwork);
    }

    /**
     * DOCUMENT ME!
     *
     * @param network DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public Device[] getAllLeafMembers(Network network) {
        Device[] devices = network.getAllDevices();

        // For faster access, use null for "all devices rule"
        if (groupingRule == null) {
            return devices;
        }

        ArrayList list = new ArrayList();

        for (int i = 0; i < devices.length; i++) {
            if (groupingRule.includeInGroup(devices[i])) {
                list.add(devices[i]);
            }
        }

        Device[] result = new Device[list.size()];
        list.toArray(result);

        list = null;

        return result;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isDynamic() {
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toConfigXMLString() {
        StringBuffer strB = new StringBuffer();

        return strB.toString();
    }
}
