SELECT HL7SegmentDataElements.seg_code, HL7SegmentDataElements.seq_no, HL7SegmentDataElements.req_opt, HL7SegmentDataElements.repetitional, HL7SegmentDataElements.repetitions FROM (HL7Versions INNER JOIN HL7SegmentDataElements ON HL7Versions.version_id = HL7SegmentDataElements.version_id) INNER JOIN HL7DataElements ON (HL7SegmentDataElements.data_item = HL7DataElements.data_item) AND (HL7SegmentDataElements.version_id = HL7DataElements.version_id) WHERE (((HL7Versions.hl7_version)=?) AND ((HL7SegmentDataElements.seg_code)=?)) ORDER BY HL7SegmentDataElements.seq_no
