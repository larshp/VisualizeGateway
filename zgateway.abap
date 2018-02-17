REPORT zgateway.

PARAMETERS: p_proj TYPE /iwbep/sbdm_project OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING /iwbep/cx_mgw_med_exception /iwbep/cx_sbcm_exception.

  TRY.
      DATA(lt_proj) = /iwbep/cl_sbdm=>get_manager( )->find_projects( VALUE #( ( sign = 'I' option = 'EQ' low = p_proj ) ) ).
      IF lines( lt_proj ) <> 1.
        RETURN.
      ENDIF.
    CATCH /iwbep/cx_sbcm_exception INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

  DATA(lo_proj) = lt_proj[ 1 ].

  LOOP AT lo_proj->/iwbep/if_sbdm_node~get_children( ) INTO DATA(li_node).
    IF li_node->get_name( ) CP '*_MPC_EXT'.
      DATA(lv_mpc) = li_node->get_name( ).
    ENDIF.
  ENDLOOP.

  DATA: li_mpc TYPE REF TO /iwbep/if_mgw_med_load.
  CREATE OBJECT li_mpc TYPE (lv_mpc).

  SELECT SINGLE * FROM /iwbep/i_mgw_ohd INTO @DATA(ls_ver) WHERE class_name = @lv_mpc.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.


  DATA: ls_header        TYPE /iwbep/if_mgw_med_odata_types=>ty_s_med_header,
        lt_nodes         TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_entity_types,
        lt_references    TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_reference,
        lt_operations    TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_functions,
        lt_text_keys     TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_texts,
        lt_documentation TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_documentation,
        lt_private_anno  TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_entity_annos,
        lt_public_anno   TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_entity_annos,
        lt_mapping       TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_mapping,
        lt_model_usages  TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_mdl_usage,
        lt_tags          TYPE /iwbep/if_mgw_med_odata_types=>ty_t_med_service_tags,
        ls_vocan         TYPE /iwbep/if_mgw_med_odata_types=>ty_s_vocan_model.


  li_mpc->load_meta_data(
    EXPORTING
      iv_version             = ls_ver-version
      iv_technical_name      = ls_ver-technical_name
    CHANGING
      cs_header              = ls_header
      ct_nodes               = lt_nodes
      ct_references          = lt_references
      ct_operations          = lt_operations
      ct_text_keys           = lt_text_keys
      ct_documentation       = lt_documentation
      ct_private_annotations = lt_private_anno
      ct_public_annotations  = lt_public_anno
      ct_mapping             = lt_mapping
      ct_model_usages        = lt_model_usages
      ct_tags                = lt_tags
      cs_vocan_model         = ls_vocan ).

  WRITE: / 'digraph {'.

  SORT lt_nodes BY name ASCENDING AS TEXT.
  LOOP AT lt_nodes INTO DATA(ls_node) WHERE type = 'R'.
    WRITE: / |{ ls_node-name };|.
  ENDLOOP.

  SORT lt_references BY name ASCENDING AS TEXT.
  LOOP AT lt_references INTO DATA(ls_reference) WHERE reference_type = 'A'.
    READ TABLE lt_nodes INTO DATA(ls_source) WITH KEY entity_id = ls_reference-source_entity_id.
    ASSERT sy-subrc = 0.
    READ TABLE lt_nodes INTO DATA(ls_target) WITH KEY entity_id = ls_reference-target_entity_id.
    ASSERT sy-subrc = 0.
    WRITE: / |{ ls_source-name } -> { ls_target-name }[label="{ ls_reference-name } { ls_reference-source_card }:{ ls_reference-target_card }"];|.
  ENDLOOP.

  WRITE: / '}'.

ENDFORM.
