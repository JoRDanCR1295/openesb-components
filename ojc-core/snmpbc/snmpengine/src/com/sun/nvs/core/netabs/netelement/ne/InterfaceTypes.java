/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

// this class encapsulates the interface types of a device
package com.sun.nvs.core.netabs.netelement.ne;

/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
  */
public class InterfaceTypes {
    /**
     * DOCUMENT ME!
     */
    public static final String[][] ifTypes = new String[][] {
            {"other", "unknown",},
            {"regular1822", "regular1822",},
            {"hdh1822", "hdh1822",},
            {"ddnX25", "ddnX25",},
            {"rfc877x25", "rfc877x25",},
            {"ethernetCsmacd", "Eth",},
            {"iso88023Csmacd", "iso88023Csmacd",},
            {"iso88024TokenBus", "iso88024TokenBus",},
            {"iso88025TokenRing", "iso88025TokenRing",},
            {"iso88026Man", "iso88026Man",},
            {"starLan", "starLan",},
            {"proteon10Mbit", "proteon10Mbit",},
            {"proteon80Mbit", "proteon80Mbit",},
            {"hyperchannel", "hyperchannel",},
            {"fddi", "fddi",},
            {"lapb", "lapb",},
            {"sdlc", "sdlc",},
            {"ds1", "ds1",},
            {"e1", "e1",},
            {"basicISDN", "basicISDN",},
            {"primaryISDN", "primaryISDN",},
            {"propPointToPointSerial", "propPointToPointSerial",},
            {"ppp", "ppp",},
            {"softwareLoopback", "Loopback",},
            {"eon", "eon",},
            {"ethernet3Mbit", "ethernet3Mbit",},
            {"nsip", "nsip",},
            {"slip", "slip",},
            {"ultra", "ultra",},
            {"ds3", "ds3",},
            {"sip", "sip",},
            {"frameRelay", "frameRelay",},
            {"rs232", "rs232",},
            {"para", "para",},
            {"arcnet", "arcnet",},
            {"arcnetPlus", "arcnetPlus",},
            {"atm", "atm",},
            {"miox25", "miox25",},
            {"sonet", "sonet",},
            {"x25ple", "x25ple",},
            {"iso88022llc", "iso88022llc",},
            {"localTalk", "localTalk",},
            {"smdsDxi", "smdsDxi",},
            {"frameRelayService", "frameRelayService",},
            {"v35", "v35",},
            {"hssi", "hssi",},
            {"hippi", "hippi",},
            {"modem", "modem",},
            {"aal5", "aal5",},
            {"sonetPath", "sonetPath",},
            {"sonetVT", "sonetVT",},
            {"smdsIcip", "smdsIcip",},
            {"propVirtual", "Vlan",},
            {"propMultiplexor", "propMultiplexor",},
            {"ieee80212", "ieee80212",},
            {"fibreChannel", "fibreChannel",},
            {"hippiInterface", "hippiInterface",},
            {"frameRelayInterconnect", "frameRelayInterconnect",},
            {"frameRelay", "frameRelay",},
            {"frameRelayService", "frameRelayService",},
            {"aflane8023", "aflane8023",},
            {"aflane8025", "aflane8025",},
            {"cctEmul", "cctEmul",},
            {"fastEther", "FastEth",},
            {"isdn", "isdn",},
            {"v11", "v11",},
            {"v36", "v36",},
            {"g703at64k", "g703at64k",},
            {"g703at2mb", "g703at2mb",},
            {"qllc", "qllc",},
            {"fastEtherFX", "fastEtherFX",},
            {"channel", "channel",},
            {"ieee80211", "Dot11Radio",},
            {"ibm370parChan", "ibm370parChan",},
            {"escon", "escon",},
            {"dlsw", "dlsw",},
            {"isdns", "isdns",},
            {"isdnu", "isdnu",},
            {"lapd", "lapd",},
            {"ipSwitch", "ipSwitch",},
            {"rsrb", "rsrb",},
            {"atmLogical", "atmLogical",},
            {"ds0", "ds0",},
            {"ds0Bundle", "ds0Bundle",},
            {"bsc", "bsc",},
            {"async", "async",},
            {"cnr", "cnr",},
            {"iso88025Dtr", "iso88025Dtr",},
            {"eplrs", "eplrs",},
            {"arap", "arap",},
            {"propCnls", "propCnls",},
            {"hostPad", "hostPad",},
            {"termPad", "termPad",},
            {"frameRelayMPI", "frameRelayMPI",},
            {"x213", "x213",},
            {"adsl", "adsl",},
            {"radsl", "radsl",},
            {"sdsl", "sdsl",},
            {"vdsl", "vdsl",},
            {"iso88025CRFPInt", "iso88025CRFPInt",},
            {"myrinet", "myrinet",},
            {"voiceEM", "voiceEM",},
            {"voiceFXO", "voiceFXO",},
            {"voiceFXS", "voiceFXS",},
            {"voiceEncap", "voiceEncap",},
            {"voiceOverIp", "voiceOverIp",},
            {"atmDxi", "atmDxi",},
            {"atmFuni", "atmFuni",},
            {"atmIma", "atmIma",},
            {"pppMultilinkBundle", "pppMultilinkBundle",},
            {"ipOverCdlc", "ipOverCdlc",},
            {"stackToStack", "stackToStack",},
            {"virtualIpAddress", "virtualIpAddress",},
            {"bgppolicyaccounting", "bgppolicyaccounting",},
            {"frf16MfrBundle", "frf16MfrBundle",},
            {"h323Gatekeeper", "h323Gatekeeper",},
            {"h323Proxy", "h323Proxy",},
            {"mpls", "mpls",},
            {"mfSigLink", "mfSigLink",},
            {"hdsl2", "hdsl2",},
            {"shdsl", "shdsl",},
            {"ds1FDL", "ds1FDL",},
            {"pos", "pos",},
            {"dvbAsiIn", "dvbAsiIn",},
            {"dvbAsiOut", "dvbAsiOut",},
            {"plc", "plc",},
            {"nfas", "nfas",},
            {"tr008", "tr008",},
            {"gr303RDT", "gr303RDT",},
            {"gr303IDT", "gr303IDT",},
            {"isup", "isup",},
            {"propDocsWirelessMaclayer", "propDocsWirelessMaclayer",},
            {"propDocsWirelessDownstream", "propDocsWirelessDownstream",},
            {"propDocsWirelessUpstream", "propDocsWirelessUpstream",},
            {"hiperlan2", "hiperlan2",},
            {"propBWAp2Mp", "propBWAp2Mp",},
            {"sonetOverheadChannel", "sonetOverheadChannel",},
            {"digitalWrapperOverheadChannel", "digitalWrapperOverheadChannel",},
            {"aal2", "aal2",},
            {"radioMAC", "radioMAC",},
            {"atmRadio", "atmRadio",},
            {"imt", "imt",},
            {"mvl", "mvl",},
            {"reachDSL", "reachDSL",},
            {"frDlciEndPt", "frDlciEndPt",},
            {"atmVciEndPt", "atmVciEndPt",},
            {"opticalChannel", "opticalChannel",},
            {"opticalTransport", "opticalTransport",},
            {"propAtm", "propAtm",},
            {"voiceOverCable", "voiceOverCable",},
            {"infiniband", "infiniband",},
            {"teLink", "teLink",},
            {"q2931", "q2931",},
            {"virtualTg", "virtualTg",},
            {"sipTg", "sipTg",},
            {"sipSig", "sipSig",},
            {"docsCableUpstreamChannel", "docsCableUpstreamChannel",},
            {"econet", "econet",},
            {"pon155", "pon155",},
            {"pon622", "pon622",},
            {"bridge", "bridge",},
            {"linegroup", "linegroup",},
            {"voiceEMFGD", "voiceEMFGD",},
            {"voiceFGDEANA", "voiceFGDEANA",},
            {"voiceDID", "voiceDID",},
        };

    /**
     * DOCUMENT ME!
     *
     * @param ifType DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getIfTyeStr(int ifType) {
        if ((ifType > ifTypes.length) || (ifType <= 0)) {
            return "unknown";
        } else {
            return ifTypes[ifType - 1][1];
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param highSpeed DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static int mapSpeedToIfTypeInt(int highSpeed) {
        switch (highSpeed) {
        case 1:
            return InterfaceCapability.CAPABILITY_ETHERNET;

        case 10:
            return InterfaceCapability.CAPABILITY_ETHERNET;

        case 100:
            return InterfaceCapability.CAPABILITY_FAST_ETHERNET;

        case 1000:
            return InterfaceCapability.CAPABILITY_GIG_ETHERNET;
        }

        ;

        return -1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param highSpeed DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String mapSpeedToIfType(int highSpeed) {
        switch (highSpeed) {
        case 1:
            return "Eth";

        case 10:
            return "Eth";

        case 100:
            return "FastEth";

        case 1000:
            return "GigEth";
        }

        ;

        return "unknown";
    }
}
