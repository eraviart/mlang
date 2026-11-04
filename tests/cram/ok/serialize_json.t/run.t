Sérialisation JSON de la MIR (F11/2 — fork eraviart/mlang, branche f11-serialiseur)

Le sérialiseur produit un JSON fidèle à la MIR exécutable et sort sans calcul.

  $ mlang errdecl.m petit.m -A app --precision double --income-year=2020 \
  >   --comparison_error_margin=0.000001 --mpp_function=orchestration \
  >   --no_nondet_display --serialize-json=mir.json
  Tag "primitive"

Le fichier produit est un JSON du format mlang-mir-json, version 1.

  $ python3 -c "
  > import json
  > d = json.load(open('mir.json'))
  > assert d['format'] == 'mlang-mir-json' and d['format_version'] == 1
  > p = d['program']
  > # structure du programme : cible principale, règles, vérifs, erreurs
  > assert p['main_target'] == 'orchestration', p['main_target']
  > assert p['rules'] == [{'id': 1, 'value': 'a_regle_1'}], p['rules']
  > assert p['verifs'] == [{'id': 1, 'value': 'a_verif_1'}], p['verifs']
  > assert list(p['errors']) == ['ATSTER'], p['errors']
  > # variables : saisie aliasée, calculee restituee, tableau à 5 cellules
  > names = {v['value']['name'] for v in p['vars_by_id']}
  > assert {'X', 'Y', 'T', 'T0', 'T1'} <= names, names
  > y = next(v['value'] for v in p['vars_by_id'] if v['value']['name'] == 'Y')
  > assert y['is_given_back'] and y['loc']['cat_str'].startswith('calculee')
  > t = next(v['value'] for v in p['vars_by_id'] if v['value']['name'] == 'T')
  > assert t['table']['size'] == 5, t['table']
  > # la règle 1 : corps si/alors/sinon avec arrondi fiscal et positif
  > regle = p['targets']['a_regle_1']
  > kinds = [i['kind'] for i in regle['prog']]
  > assert kinds == ['affectation'], kinds
  > aff = regle['prog'][0]
  > assert aff['formula']['value']['type'] == 'single'
  > e = aff['formula']['value']['decl']['expr']
  > assert e['kind'] == 'si_expression', e['kind']
  > assert e['cond']['kind'] == 'comparison' and e['cond']['op']['value'] == 'gt'
  > alors = e['then']
  > assert alors['kind'] == 'binop' and alors['op']['value'] == 'add'
  > assert alors['lhs']['kind'] == 'funcall'
  > assert alors['lhs']['func']['value'] == 'arr'
  > # la vérif 1 : condition sur variable saisie, erreur portée
  > verif = p['targets']['a_verif_1']
  > vi = verif['prog'][0]
  > assert vi['kind'] == 'si' and vi['then'][0]['kind'] == 'erreur'
  > assert vi['then'][0]['error']['value'] == 'ATSTER'
  > # orchestration : calculer domaine + verifier domaine + exporte_erreurs
  > orch = p['targets']['orchestration']
  > okinds = [i['kind'] for i in orch['prog']]
  > assert okinds == ['calculer_cible', 'calculer_cible', 'exporte_erreurs'], okinds
  > assert orch['prog'][0]['target']['value'].endswith('rules_primitive')
  > assert orch['prog'][1]['target']['value'].startswith('a_verifs_primitive')
  > # provenance des options numériques
  > pr = d['provenance']
  > assert pr['income_year'] == 2020 and pr['value_sort'] == 'double'
  > assert pr['round_ops'] == 'default' and pr['comparison_error_margin'] == 1e-06
  > print('OK serialize_json')
  > "
  OK serialize_json
