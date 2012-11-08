/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.netabs.netelement.ne;

import com.sun.nvs.core.netabs.netelement.ne.IpAddress;
import com.sun.nvs.core.util.*;

//import com.sun.nvs.core.north.cli.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.TreeSet;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class IpAddressList implements Comparator {
    private static final int[] colWidths = {12, 15, 15,};
    private static final String[] colHeading = {"Index", "From Ip", "To Ip",};
    private String _name;

    //To store effective/merged IP range List
    private ArrayList ipList = new ArrayList(4);

    //To store indexed ip-ranges in a sorted TreeSet
    private TreeSet ipSet = null;

    //Boolean variable to allow the user to define if two address-ranges
    //can have the same IP address
    private boolean _isOverlapAllowed = true;
    private String[] cols = new String[3];

    /**
     * Creates a new IpAddressList object.
     *
     * @param name DOCUMENT ME!
     * @param overlapOfAddressAllowed DOCUMENT ME!
     */
    public IpAddressList(String name, boolean overlapOfAddressAllowed) {
        _isOverlapAllowed = overlapOfAddressAllowed;
        _name = name;
        ipSet = new TreeSet(this);
    }

    /**
     * Creates a new IpAddressList object.
     *
     * @param name DOCUMENT ME!
     */
    public IpAddressList(String name) {
        this(name, true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param overlapOfAddressAllowed DOCUMENT ME!
     */
    public void setOverlap(boolean overlapOfAddressAllowed) {
        int index = isListUnique();

        if (index < 0) {
            _isOverlapAllowed = overlapOfAddressAllowed;
        } else {
            throw new IllegalArgumentException(
                "This condition can not be set as the IpAddress List defined already has over-lapping ranges at: " +
                index
            );
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isOverlapAllowed() {
        return _isOverlapAllowed;
    }

    /**
     * DOCUMENT ME!
     *
     * @param o1 DOCUMENT ME!
     * @param o2 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int compare(Object o1, Object o2) {
        IpAddressRange ipR1 = (IpAddressRange) o1;
        IpAddressRange ipR2 = (IpAddressRange) o2;

        return (ipR1.getIndex() - ipR2.getIndex());
    }

    /**
     * DOCUMENT ME!
     *
     * @param r1 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddressRange addToIndexedList(IpAddressRange r1) {
        IpAddressRange rTemp = getByIndexFromIpSet(r1.getIndex());

        if (rTemp == null) {
            if (_isOverlapAllowed) {
                ipSet.add(r1);
            } else {
                int index = isUniqueRange(r1);

                if (index < 0) {
                    ipSet.add(r1);
                } else {
                    throw new IllegalArgumentException(
                        "IP Range specified overlaps with the existing address ranges defined at index " +
                        index
                    );
                }
            }

            buildMergedIpList();

            return r1;
        } else {
            rTemp.setIpRange(r1.getLowValue(), r1.getHighValue());
            buildMergedIpList();

            return rTemp;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int isListUnique() {
        ArrayList ipListTemp = getIpSetAsList();

        int sz = ipListTemp.size();

        for (int i = 0; i < sz; i++) {
            IpAddressRange rec1 = (IpAddressRange) ipListTemp.get(i);

            for (int j = i + 1; j < sz; j++) {
                IpAddressRange rec2 = (IpAddressRange) ipListTemp.get(j);

                if (rec1.intersects(rec2)) {
                    return rec2.getIndex();
                } else {
                    continue;
                }
            }
        }

        return -1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param rec1 DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int isUniqueRange(IpAddressRange rec1) {
        Iterator itr = ipSet.iterator();

        while (itr.hasNext()) {
            IpAddressRange rec2 = (IpAddressRange) itr.next();

            if (rec1.intersects(rec2)) {
                return rec2.getIndex();
            } else {
                continue;
            }
        }

        return -1;
    }

    private void recursiveMerge() {
        int sz = ipList.size();

        for (int i = 0; i < sz; i++) {
            IpAddressRange rec1 = (IpAddressRange) ipList.get(i);

            for (int j = i + 1; j < sz; j++) {
                IpAddressRange rec2 = (IpAddressRange) ipList.get(j);
                IpAddressRange rNew = checkList(rec1, rec2);

                //System.err.println("in for: "+i+" "+j);
                if (rNew != null) {
                    //System.err.println("Found Rec at"+j);
                    ipList.remove(j);
                    ipList.remove(i);
                    i--; // To make sure we don't skip a recod in the original list
                    ipList.add(ipList.size(), rNew);

                    break;
                } else {
                    continue;
                }
            }

            sz = ipList.size();
        }
    }

    //If null is returned the two address ranges do ot overlap
    private IpAddressRange checkList(IpAddressRange r1, IpAddressRange r2) {
        return (r1.union(r2));
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ArrayList getEffectiveList() {
        return ipList;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ArrayList getIpSetAsList() {
        int sz = ipSet.size();
        ArrayList tempList = new ArrayList();
        Iterator itr = ipSet.iterator();

        while (itr.hasNext()) {
            IpAddressRange ipR = (IpAddressRange) (itr.next());
            tempList.add(ipR);
        }

        return tempList;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int size() {
        return ipSet.size();
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isValidIp(IpAddress ip) {
        int sz = ipList.size();

        for (int i = 0; i < sz; i++) {
            IpAddressRange r1 = (IpAddressRange) ipList.get(i);

            if (r1.isInRange(ip)) {
                return true;
            } else {
                continue;
            }
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean isManageableIp(IpAddress ip) {
        if (isValidIp(ip)) {
            int index = getIndexForIp(ip);

            if (index >= 0) {
                return (getByIndexFromIpSet(index).isManageable());
            }
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @param ind DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean clearFromIpSet(int ind) {
        IpAddressRange ipR = getByIndexFromIpSet(ind);

        if (ipR != null) {
            ipSet.remove(ipR);
            buildMergedIpList();

            return true;
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param ind DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public IpAddressRange getByIndexFromIpSet(int ind) {
        Iterator itr = ipSet.iterator();

        if (ind >= 0) {
            while (itr.hasNext()) {
                IpAddressRange ipR = (IpAddressRange) (itr.next());

                if (ipR.getIndex() == ind) {
                    return ipR;
                }
            }
        }

        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param index DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public boolean hasIndex(int index) {
        return (getByIndexFromIpSet(index) != null);
    }

    /**
     * DOCUMENT ME!
     *
     * @param ip DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getIndexForIp(IpAddress ip) {
        Iterator itr = ipSet.iterator();

        while (itr.hasNext()) {
            IpAddressRange ipR = (IpAddressRange) (itr.next());

            if (ipR.isInRange(ip)) {
                return ipR.getIndex();
            }
        }

        return -1;
    }

    private void buildMergedIpList() {
        Iterator itr = ipSet.iterator();

        ipList.clear();

        while (itr.hasNext()) {
            IpAddressRange ipR = (IpAddressRange) (itr.next());
            ipList.add(ipR);
        }

        recursiveMerge();
    }

    private void addToList(IpAddressRange r1) {
        int sz = ipList.size();
        IpAddressRange rNew = null;

        for (int i = 0; i < sz; i++) {
            //System.out.println("For Loop - Adding to List: "+sz);
            rNew = checkList(((IpAddressRange) ipList.get(i)), r1);

            if (rNew != null) {
                //System.out.println("Found Rec "+sz);
                //System.err.println("---Update Recs: Before ADD/Remove"+printList());
                ipList.remove(i);
                ipList.add(i, rNew);
                //System.err.println("---Update Recs: After ADD/Remove"+printList());
                recursiveMerge();

                //System.err.println("---Update Recs: After Merge"+printList());
                return;
            } else {
                System.err.println("Null found");
            }
        }

        if (rNew == null) {
            ipList.add(r1);
            System.out.println("New Record - Adding to List: " + sz);

            return;
        }
    }

    private void clearIp(IpAddress ip1, IpAddress ip2) {
        IpAddressRange r2 = new IpAddressRange(ip1, ip2);

        int sz = ipList.size();

        for (int i = 0; i < sz; i++) {
            IpAddressRange r1 = (IpAddressRange) ipList.get(i);
            IpAddressRange[] rNew = r1.excludeRange(r2);

            if (r1.equals(r2)) {
                ipList.remove(i);

                return;
            }

            if (rNew != null) {
                if (rNew.length != 0) {
                    if (rNew[0] != null) {
                        ipList.remove(i);
                        ipList.add(i, rNew[0]);
                        System.err.println("add rec at index " + (ipList.size() - 1));

                        //printList();
                    }

                    if (rNew.length == 2) {
                        ipList.add(i + 1, rNew[1]);
                        i = i + 1;
                        System.err.println("add rec at index " + (ipList.size() - 1));
                        recursiveMerge();

                        //printList();
                        return;
                    } else {
                        continue;
                    }
                }
            }
        }

        //ipList.clear();
    }
}
