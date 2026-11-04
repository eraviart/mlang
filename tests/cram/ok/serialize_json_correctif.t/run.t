Sérialisation JSON des phases correctif et rappels (F11/A1 — ticket #101)

Un programme qui porte la branche « si MODE_CORR + 0 = 1 » est émis en
format 2 : sections phases / ref_frames / evenements, et annotations de
résolution sur les instructions de la machinerie (iterer, restaurer,
arranger_evenements, quand_puis_quand).

  $ mlang errdecl.m correctif.m -A app --precision double --income-year=2020 \
  >   --comparison_error_margin=0.000001 --mpp_function=orchestration \
  >   --no_nondet_display --serialize-json=mir2.json

  $ python3 -c "
  > import json
  > d = json.load(open('mir2.json'))
  > assert d['format'] == 'mlang-mir-json' and d['format_version'] == 2
  > assert {'phases', 'ref_frames', 'evenements'} <= set(d), list(d)
  > p = d['program']
  > # -- phases : correctif = branche « alors » de la cible principale
  > corr = d['phases']['correctif']
  > assert corr['present'] and corr['branche'] == 'alors', corr
  > assert corr['variable_selecteur'] == 'MODE_CORR', corr
  > assert corr['cible_principale'] == 'orchestration', corr
  > assert corr['cibles'] == ['corr'], corr['cibles']
  > assert corr['quand_puis_quand'] == 1, corr['quand_puis_quand']
  > assert corr['pos']['file'] == 'correctif.m', corr['pos']
  > # -- rappels : cibles qui manipulent les événements
  > rapp = d['phases']['rappels']
  > assert rapp['present'] and rapp['cibles'] == ['corr'], rapp
  > # -- ref_frames : frame de la cible correctif et emplacements empilés
  > rf = d['ref_frames']
  > assert set(rf['semantique']) >= {'base', 'acces', 'liaison', 'empilement',
  >     'taille', 'indices_negatifs', 'pile_globale'}, list(rf['semantique'])
  > assert rf['nb_all_refs'] >= 1, rf['nb_all_refs']
  > cadre = rf['cibles']['corr']
  > assert cadre['taille'] == 1 and cadre['empile_a_entree'] == 1, cadre
  > assert len(cadre['emplacements']) == 1, cadre['emplacements']
  > emp = cadre['emplacements'][0]
  > # indice négatif : compté depuis la base empilée à l'entrée de la cible
  > assert emp['idx'] == -1, emp['idx']
  > # le même emplacement porte deux liaisons (iterer puis restaurer) : pile
  > genres = sorted(l['genre'] for l in emp['liaisons'])
  > assert genres == ['iteration', 'restauration'], genres
  > assert [l['nom'] for l in emp['liaisons']] == ['VX', 'VX'], emp['liaisons']
  > # -- variables de référence : résolues vers cible + emplacement + liaison
  > refs = [v['value'] for v in p['vars_by_id'] if v['value']['scope'] == 'ref']
  > assert len(refs) == 2, len(refs)
  > for v in refs:
  >     assert v['loc']['kind'] == 'ref' and v['loc']['idx'] == -1, v['loc']
  >     assert v['frame']['cible'] == 'corr' and v['frame']['idx'] == -1, v['frame']
  >     assert v['frame']['genre'] in ('iteration', 'restauration'), v['frame']
  >     assert v['frame']['liaison_pos']['file'] == 'correctif.m', v['frame']
  > # -- événements déclaratifs : déclaration du prélude DGFiP
  > ev = d['evenements']
  > champs = {c['field']: c for c in ev['declaration']['champs']}
  > assert len(champs) == 12, sorted(champs)
  > assert champs['code']['is_var'] and not champs['montant']['is_var'], champs
  > assert champs['numero']['index'] == 0 and champs['strate']['index'] == 11, champs
  > assert set(ev['semantique']) >= {'pile', 'champ_evenement', 'affectation_champ',
  >     'nb_evenements', 'arranger_evenements', 'restaurer'}, list(ev['semantique'])
  > ops = ev['cibles']['corr']
  > assert len(ops['arranger_evenements']) == 1, ops
  > assert len(ops['champ_evenement']) == 4, ops['champ_evenement']
  > assert len(ops['nb_evenements']) == 1, ops['nb_evenements']
  > assert len(ops['affecte_champ_evenement']) == 1, ops
  > assert ops['restaurer_evenements'] == [], ops['restaurer_evenements']
  > # -- annotations de résolution sur les instructions
  > prog = p['targets']['corr']['prog']
  > kinds = [i['kind'] for i in prog]
  > assert kinds == ['iterer', 'restaurer', 'arranger_evenements',
  >     'quand_puis_quand'], kinds
  > assert prog[0]['frame'] == {'idx': -1, 'genre': 'iteration'}, prog[0]['frame']
  > assert prog[1]['restauration']['parametres_variables'] == 1, prog[1]
  > assert prog[2]['pile_evenements'] == {'empile': True, 'tri': True,
  >     'filtre': True, 'ajout': True}, prog[2]['pile_evenements']
  > assert prog[3]['semantique']['nb_branches'] == 2, prog[3]['semantique']
  > # -- sections par cible : frame et opérations d'événements
  > cible = p['targets']['corr']
  > assert cible['nb_refs'] == cible['ref_frame']['taille'] == 1, cible['nb_refs']
  > assert cible['evenements']['arranger_evenements'][0]['sline'] == 25, cible
  > prim = p['targets']['prim']
  > assert prim['ref_frame']['taille'] == 0 and prim['ref_frame']['emplacements'] == [], prim
  > print('OK serialize_json_correctif')
  > "
  OK serialize_json_correctif

Rétrocompatibilité : un programme sans branche MODE_CORR reste au format 1,
sans aucune des sections nouvelles (les documents primitifs 2020/2021 sont
inchangés octet pour octet).

  $ mlang errdecl.m primitif.m -A app --precision double --income-year=2020 \
  >   --comparison_error_margin=0.000001 --mpp_function=orchestration \
  >   --no_nondet_display --serialize-json=mir1.json

  $ python3 -c "
  > import json
  > d = json.load(open('mir1.json'))
  > assert d['format_version'] == 1, d['format_version']
  > assert not ({'phases', 'ref_frames', 'evenements'} & set(d)), list(d)
  > cible = d['program']['targets']['orchestration']
  > assert 'ref_frame' not in cible and 'evenements' not in cible, list(cible)
  > assert [i['kind'] for i in cible['prog']] == ['affectation'], cible['prog']
  > print('OK serialize_json_primitif_inchange')
  > "
  OK serialize_json_primitif_inchange
